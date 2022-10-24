############################Libraries########################
############################################################

suppressMessages(pacman::p_load(raster, sp, sf, terra, tidyverse, 
                                blockCV, caret, RStoolbox, randomForest,
                                pROC, dplyr))

############################################################

aoi_r = shapefile(poly_path) ###### an sp polygon of your study areas
poly_sf = st_as_sf(aoi_r)    ###### convert the polygon to sf for later
boss_sf = read.csv(boss_path) %>%   ###### any csv with lat lon that has your samples and rasters extracted at those points 
          sf::st_as_sf(wkt = 'geometry', crs=4326)  ###### can use 'coords = c("x", "y")' instead of wkt if there is no geometry colomn


############################Raster read in########################
#################################################################
# Read in and crop the rasters to your study extent
# the rasters should be the same as the as the values in the shapefile
sdb = brick(sdb_path) %>% 
      raster::crop(aoi_r) %>%
      raster::mask(aoi_r)
 
bands = brick(band_path) %>%
        raster::crop(aoi_r) %>%
        raster::mask(aoi_r)


stack = brick(addLayer(bands,sdbi)) ### raster stack for predictions
names(stack) = c('SDB','B1') ## names must be consistent with your csv
rpc = rasterPCA(stack, nComp = 1 , spca = TRUE, nSamples = 5000) ### first PCA component of the raster stack to estimate spatial AC

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

print(range1) ###### use this to inform the block size in 'the range argument below"
 
# creating the spatial blocks
sb1 = spatialBlock(speciesData = boss_sf,
                   species = "Class", ####### change this to the label of your csv (eg. seagrass, algae)
                   rasterLayer = rpc$map,
                   theRange = 10000,
                   k = 5, ####### suggest using 5 folds 
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
         dplyr::mutate(class_int = as.factor(class_int)) %>%  ##### make sure your label is factor in the dataframe
         drop_na(B1, SDB) ## drop any nas in your covariates

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
for(i in 1:5){
  #tryCatch({
  #print(i)
  train = subset(mod_df, mod_df$Block != as.character(i))
  fac_train = factor(train$Cluster) #### change this to the label of your response var
  train$Cluster = fac_train
  test = subset(mod_df, mod_df$Block == as.character(i))
  fac_test = factor(test$Cluster) #### change this to the label of your response var
  test$Cluster =  fac_test
  test = rbind(train[1, ] , test)
  test = test[-1,]
  
  rf_c = randomForest(Cluster ~ SDB + B1,
                      data = train,
                      na.action=na.omit,
                      ntree = 1000,
                      maxnodes = 5, ###### this is really the most important thing to adjust. It controls the depth of the tree
                      importance=TRUE, ###### tru between 5 and 12 at most, and inform from the kappa outcomes below
                      proximity=FALSE,
                      xtest = data.frame(
                        test$ndavi, test$SDB ##### make sure these are the same as your covariates
                      ), 
                      ytest = test$Cluster,
                      keep.forest = TRUE) 
  
  train_pred = predict(rf_c, train)
  conf_train = confusionMatrix(train_pred, train$Cluster)
  
  test_pred = predict(rf_c, test)
  conf_test = confusionMatrix(test_pred, test$Cluster)
  
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


#######################CV summary#########################
tab = t(data.frame(oob,kapp_train, test_err, kapp_test))
mean_st = (as.numeric(rowMeans(tab)))
row.names(tab) = c('OOB error', 'Kappa (training)', 'Test error', 'Kappa (testing)')
colnames(tab) = c('Fold-1', 'Fold-2', 'Fold-3','Fold-4', 'Fold-5')
tab_df = tibble::rownames_to_column(as.data.frame(tab), "Statistic")
tab_df$Mean = mean_st
tab_df

############################################################

#######################raster predictions#########################
pred_class = raster::predict(stack,rf_c, type="response") 
#%>%focal(w=matrix(1,3,3), fun=modal)) can smooth out the preditions if needed
pred_prob = raster::predict(k,rf_c, type="prob") ### for class probabilities

