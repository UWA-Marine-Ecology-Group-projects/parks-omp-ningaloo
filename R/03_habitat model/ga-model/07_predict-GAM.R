###
# Project: Parks Ningaloo
# Data:    BRUVS, BOSS Habitat data
# Task:    Merging habitat data
# Author:  Claude Spencer
# Date:    Feb 2023
##

rm(list = ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(terra)
library(raster)
library(dismo)
library(tidyverse)
library(sf)

name <- "Parks-Ningaloo-synthesis"

# read in
habi   <- readRDS(paste(paste0('data/tidy/', name), 
                        'ga_habitat-bathy-derivatives.rds', sep = "_"))
preds  <- readRDS(paste(paste0('data/spatial/rasters/', name), 
                        'ga_spatial_covariates.rds', sep = "_"))
plot(preds)

# xy <- habi %>%
#   dplyr::select(x , y) %>%
#   glimpse()
# 
# dat <- raster::extract(preds[[c(1, 6, 7)]], xy)
# messrast <- mess(preds[[c(1, 6, 7)]], dat) %>%
#   clamp(lower = -0.01, useValues = F)
# plot(messrast)

preddf <- as.data.frame(preds, xy = TRUE, na.rm = TRUE)

# use formula from top model from '06_GAM_modelselect.R'
m_sand <- gam(cbind(sand, broad.total.points.annotated - sand) ~ 
                 s(detrended,     k = 5, bs = "cr")  + 
                 s(roughness, k = 5, bs = "cr") + 
                 s(Z, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)

m_inverts <- gam(cbind(inverts, broad.total.points.annotated - inverts) ~ 
                s(detrended,     k = 5, bs = "cr")  + 
                s(roughness, k = 5, bs = "cr") + 
                s(Z, k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"), 
              correlation = corSpatial(form = ~x + y))
summary(m_inverts)

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "psand" = predict(m_sand, preddf, type = "response"),
                "pinverts" = predict(m_inverts, preddf, type = "response")) %>%
  # dplyr::filter(Z < -20) %>%
  glimpse()

prasts <- rasterFromXYZ(preddf %>% dplyr::select(x, y, starts_with("p")),
                        crs = crs(preds))
plot(prasts)
summary(prasts)

# messrast <- crop(messrast, prasts)
# prasts_m <- mask(prasts, messrast)
# plot(prasts_m)
preddf <- as.data.frame(prasts, xy = T, na.rm = T)
# categorise by dominant tag
preddf$dom_tag <- apply(preddf[3:4], 1,
                         FUN = function(x){names(which.max(x))})
preddf$dom_tag <- sub('.', '', preddf$dom_tag)
head(preddf)

saveRDS(preddf, paste0("output/gam-habitat/", name, "_predicted-habitat.rds"))

# Some little bits for Tim's workshop
# Save out as a raster
raster::writeRaster(prasts[[1]], paste0("output/gam-habitat/",name,"_predicted_sand.tif"),
                    overwrite = T)

raster::writeRaster(prasts[[2]], paste0("output/gam-habitat/",name,"_predicted_inverts.tif"),
                    overwrite = T)

# Convert it to a shapefile
dom_rast <- preddf %>%
  dplyr::select(x, y, dom_tag) %>%
  dplyr::mutate(dom_tag = dplyr::recode(dom_tag,
                                        "sand" = "1",
                                        "inverts" = "2")) %>%
  rast(type = "xyz", crs = "epsg:4326") 
plot(dom_rast)
# Don't think I need this step?
pred_stars <- st_as_stars(dom_rast)

dom.habs <- st_as_sf(pred_stars, as_points = FALSE, merge = TRUE) %>%
  dplyr::mutate(dom_tag = dplyr::recode(dom_tag,
                "1" = "sand",
                "2" = "inverts")) 
plot(dom.habs)

st_write(dom.habs, paste0("output/habitat/", name, "_predicted-dominant-habitat.shp"),
         append = F)
