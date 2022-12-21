###
# Project: Parks OMP Ningaloo
# Data:    BRUVS, BOSS Habitat data
# Task:    Model habitat
# Author:  Claude Spencer & Stanley Mastrantonis
# Date:    October 2022
##


# Clear your environment
rm(list = ls())

# Load libraries
library(randomForest)
library(tidyverse)
library(raster)
library(terra)
library(sp)
library(sf)
sf_use_s2(T)
library(ggplot2)
library(RStoolbox)
library(blockCV)
library(caret)
library(pROC)
library(ggnewscale)
library(scales)

# Set your study name
name <- "Parks-Ningaloo-synthesis"                                              # Change here

# Load data
dat <- readRDS(paste(paste0('data/tidy/', name), 
                      'nesp-habitat-bathy-derivatives.rds', sep = "_")) %>%
  dplyr::mutate(dom_tag = ifelse((inverts/broad.total.points.annotated) > 0.2, "inverts", "sand")) %>%
  # dplyr::mutate(dom_tag = ifelse((inverts/broad.total.points.annotated) > (broad.consolidated/broad.total.points.annotated),
  #                                "inverts", ifelse(broad.consolidated > 0, "rock", "sand"))) %>%
  # dplyr::mutate(dom_tag = ifelse((inverts/broad.total.points.annotated) < 0.2 & 
  #                                  (broad.consolidated/broad.total.points.annotated) < 0.2, "sand", dom_tag)) %>%
  glimpse()

test <- dat %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(n = n(),
                   max.depth = min(Z),
                   min.depth = max(Z))

unique(dat$dom_tag)
# dat$dom_tag <- apply(dat %>% dplyr::select(sand, inverts), 1,
#                                         FUN = function(x){names(which.max(x))})

###########################################################

############################Raster read in########################
#################################################################
# Read in and crop the rasters to your study extent
# the rasters should be the same as the as the values in the shapefile

stack <- readRDS(paste(paste0('data/spatial/rasters/raw bathymetry/', name),      # This is ignored - too big!
                       'spatial_covariates.rds', sep = "_")) %>%
  rast() %>%
  brick()
rpc = rasterPCA(stack, nComp = 1 , spca = TRUE, nSamples = 5000) ### first PCA component of the raster stack to estimate spatial AC

poly_sf <- st_sf(geometry = st_as_sfc(st_bbox(stack)))   ###### convert the polygon to sf for later
aoi_r <- as(object = poly_sf, Class = "Spatial") ###### an sp polygon of your study areas
boss_sf <- dat %>%   ###### any csv with lat lon that has your samples and rasters extracted at those points 
  sf::st_as_sf(coords = c("x", "y"), crs = "+proj=utm +zone=49 +south +datum=WGS84 +units=m +no_defs")  ###### can use 'coords = c("x", "y")' instead of wkt if there is no geometry colomn

############################################################


############################Setting up CV folds########################
######################################################################
# AC range estimation
range1 = spatialAutoRange(
  rpc$map,
  sampleNumber = 5000L,
  border = poly_sf,
  speciesData = NULL,
  doParallel = TRUE,
  nCores = 5,
  showPlots = TRUE,
  maxpixels = 1e+05,
  plotVariograms = TRUE,
  progress = TRUE
)

print(range1$range) ###### use this to inform the block size in 'the range argument below"

# creating the spatial blocks
sb1 = spatialBlock(speciesData = boss_sf,
                   species = "dom_tag", # change this to the label of your csv (eg. seagrass, algae)
                   rasterLayer = rpc$map,
                   theRange = 5000, # Decreased range so we have data in all folds
                   k = 5, #suggest using 5 folds 
                   selection = "random",
                   iteration = 100,
                   numLimit = NULL,
                   biomod2Format = TRUE,
                   xOffset = 0.3,
                   progress = FALSE,
                   seed = 1,
                   yOffset = 0)

# creating a dataframe for the RF model 

mod_df = as_Spatial(boss_sf) %>%
  data.frame() %>%
  add_column(Block = sb1$foldID) %>%
  dplyr::mutate(dom_tag = as.factor(dom_tag)) %>%  ##### make sure your label is factor in the dataframe
  drop_na(roughness) ## drop any nas in your covariates

###################RF models##########################################
#####################################################################

oob = NULL
test_err = NULL
kapp_train = NULL
kapp_test = NULL
auc_train_val = list()
auc_test_val = list()
roc_train = list()
roc_test = list()
i = 1
for(i in 1:5){ # works if k = 5
  #tryCatch({
  #print(i)
  train = subset(mod_df, mod_df$Block != as.character(i))
  fac_train = factor(train$dom_tag) #### change this to the label of your response var
  train$dom_tag = fac_train
  test = subset(mod_df, mod_df$Block == as.character(i))
  fac_test = factor(test$dom_tag) #### change this to the label of your response var
  test$dom_tag =  fac_test
  test = rbind(train[1, ] , test)
  test = test[-1,]
  
  rf_c = randomForest(dom_tag ~ Z + detrended + roughness, # slope + aspect + TPI + TRI + roughness +
                      data = train,
                      na.action=na.omit,
                      ntree = 1000,
                      maxnodes = 5, # this is really the most important thing to adjust. It controls the depth of the tree
                      importance=TRUE, # tru between 5 and 12 at most, and inform from the kappa outcomes below
                      proximity=FALSE,
                      xtest = data.frame(test$Z, test$detrended, test$roughness ## make sure these are the same as your covariates test$slope, test$aspect, test$TPI, test$TRI, test$roughness,
                      ), 
                      ytest = test$dom_tag,
                      keep.forest = TRUE) 
  
  train_pred = predict(rf_c, train)
  conf_train = confusionMatrix(train_pred, train$dom_tag)
  
  test_pred = predict(rf_c, test)
  conf_test = confusionMatrix(test_pred, test$dom_tag)
  
  kapp_train[i] = conf_train$overall[1]
  oob[i] = mean(rf_c$err.rate[,1])
  
  kapp_test[i] = conf_test$overall[1]
  test_err[i] = mean(rf_c$test$err.rate[,1])
  
  #n =1
  auc_nest_train = list()
  auc_nest_test = list()
  roc_nest_train = list()
  roc_nest_test = list()
  
}
############################################################

imp.plot <- varImpPlot(rf_c)

imp <- importance(rf_c)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
par(mfrow=c(3, 1))
for (i in seq_along(impvar)) {
  partialPlot(rf_c, train, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]))
}
par(mfrow = c(1,1)) # Switch back to single plot


##### ROC ######
# A measure between true positive predictions (correct) and false postive prediciton (incorrect) 
predictions = as.data.frame(predict(rf_c, mod_df, type = "prob")) # predicting the prob of each class over the og df
predictions$predict = names(predictions)[1:2][apply(predictions[,1:2], 1, which.max)] # threshold for classification
predictions$observed = mod_df$dom_tag # actual classes


roc.invert = roc(ifelse(predictions$observed == "inverts", "inverts", "non-inverts"), as.numeric(predictions$inverts))

png(filename = paste0("figures/habitat/", name, "_ROC-curve.png"),
    width = 8, height = 6, res = 300, units = "in")
plot(roc.invert, col = "red") # ,main ='ROC Curve for Inverts'
text(x=1.0, y=0.95, labels=paste("AUC =", round(roc.invert$auc,2)))
dev.off()

#######################CV summary#########################
tab = t(data.frame(oob,kapp_train, test_err, kapp_test))
mean_st = (as.numeric(rowMeans(tab)))
row.names(tab) = c('OOB error', 'Kappa (training)', 'Test error', 'Kappa (testing)')
colnames(tab) = c('Fold-1', 'Fold-2', 'Fold-3','Fold-4', 'Fold-5')
tab_df = tibble::rownames_to_column(as.data.frame(tab), "Statistic")
tab_df$Mean = mean_st
tab_df

write.csv(tab_df, file = paste0("output/rf-habitat/", name, "_nesp_model-kappa.csv"),
          row.names = F)

############################################################

#######################raster predictions#########################
pred_class = raster::predict(stack,rf_c, type = "response") 
#%>%focal(w=matrix(1,3,3), fun=modal)) can smooth out the preditions if needed
pred_prob = raster::predict(stack,rf_c, type = "prob") ### for class probabilities

plot(pred_class)
plot(pred_prob)

writeRaster(pred_class, filename = paste0("output/rf-habitat/", name, "_nesp_predicted-habitat.tif"),
            overwrite = T)

saveRDS(pred_class, file = "output/rf-habitat/ningaloo-habitat-spatial_UTM49.rds")
