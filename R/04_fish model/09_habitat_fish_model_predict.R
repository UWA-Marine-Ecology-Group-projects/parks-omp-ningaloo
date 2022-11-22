###
# Project: Parks OMP Ningaloo
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat-Fish modelling + Prediction
# author:  Claude Spencer
# date:    November 2022
##

rm(list=ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)
library(stringr)

name <- "Parks-Ningaloo-synthesis"  # set study name

# read in
dat1 <- readRDS(paste(paste0('data/tidy/', name), 
                      'gam-abundance.rds', sep = "_")) %>%
  dplyr::filter(!is.na(mean.relief)) %>%
  dplyr::rename(number = maxn) %>%                                              # Rename both to the same to join
  glimpse()

dat2 <- readRDS(paste(paste0('data/tidy/', name), 
                      'gam-length.rds', sep = "_")) %>%
  dplyr::filter(!is.na(mean.relief)) %>%
  glimpse()

fabund <- bind_rows(dat1,dat2)                                                  # Merged fish data from 02_fish_abundance.R & 03_fish_length.R

preddf <- readRDS(paste(paste0('data/spatial/rasters/raw bathymetry/', name), 
                        'spatial_covariates.rds', sep = "_")) %>%
  rast() %>%
  as.data.frame(xy = T, na.rm = T) %>%
  dplyr::rename(depth = Z) %>%
  glimpse()

pred_class <- rast(paste0("output/rf-habitat/", name, "_nesp_predicted-habitat.tif"))  

presp <- vect(preddf, geom = c("x", "y"))
preddf <- cbind(terra::extract(pred_class, presp), preddf) %>%
  dplyr::rename(habitat.class = category)
names(preddf)

# Don't need to join these, no top model contains relief
# prel   <- readRDS("output/predicted_relief_raster.rds")                         # predicted relief from 'R/habitat/5_krige_relief.R'

# join habitat and relief predictions
# predsp <- SpatialPointsDataFrame(coords = cbind(preds$x, preds$y), data = preds)
# predsp$mean.relief <- raster::extract(prel, predsp)
# preddf        <- as.data.frame(predsp, xy = TRUE, na.rm = TRUE)
# preddf$depth  <- preddf$Z * -1
# preddf$rock   <- preddf$prock
# preddf$biog   <- preddf$pbiogenic
# preddf$macroalgae   <- preddf$pmacroalg
# head(preddf)

########################## Delete this next bit? ##########################

# reduce predictor space to fit survey area
# fishsp <- SpatialPointsDataFrame(coords = cbind(fabund$longitude.1, 
#                                                 fabund$latitude.1), 
#                                  data = fabund)
# sbuff  <- buffer(fishsp, 10000)
# unique(fabund$scientific)

# use formula from top model from FSSGam model selection
# Total abundance
m_totabund <- gam(number ~ s(detrended, k = 3, bs = "cr"), 
               data = fabund%>%dplyr::filter(scientific%in%"total.abundance"), 
               method = "REML", family = tw())
summary(m_totabund)

# Species richness
m_richness <- gam(number ~ s(depth, k = 3, bs = "cr") + habitat.class,  
                     data = fabund%>%dplyr::filter(scientific%in%"species.richness"), 
                     method = "REML", family = tw())
summary(m_richness)

# Greater than legal size target species
m_legal <- gam(number ~ s(detrended, k = 3, bs = "cr"),  
                  data = fabund%>%dplyr::filter(scientific%in%"greater than legal size"), 
                  method = "REML", family = tw())
summary(m_legal)

# Smaller than legal size target species
m_sublegal <- gam(number ~ s(depth, k = 3, bs = "cr"),  
               data = fabund%>%dplyr::filter(scientific%in%"smaller than legal size"), 
               method = "REML", family = tw())
summary(m_sublegal)


# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_totabund" = predict(m_totabund, preddf, type = "response"),
                "p_richness" = predict(m_richness, preddf, type = "response"),
                "p_legal" = predict(m_legal, preddf, type = "response"),
                "p_sublegal" = predict(m_sublegal, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf[, c(3, 4, 13:16)]) 
plot(prasts)

## No need for this anymore, just used the masked habitat predictions to start with
# subset to 10km from sites only 
# sprast <- mask(prasts, sbuff)
# plot(sprast)

dev.off()
plot(prasts$p_totabund)
plot(prasts$p_richness)
plot(prasts$p_legal)
plot(prasts$p_sublegal)

# tidy and output data
spreddf <- as.data.frame(prasts, xy = TRUE, na.rm = TRUE)                       # Back to df again lol

summary(spreddf)                                                                # Mad outliers in detrended bathy, gonna have to suss it out

# saveRDS(preddf, "output/broad_fish_predictions.rds")
saveRDS(spreddf, "output/fssgam-fish/site_fish_predictions.rds")
