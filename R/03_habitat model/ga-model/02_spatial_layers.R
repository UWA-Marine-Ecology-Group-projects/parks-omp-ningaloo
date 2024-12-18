###
# Project: Parks OMP Ningaloo
# Data:    Bathymetry Data
# Task:    Prepare spatial layers for modelling
# Author:  Claude Spencer
# Date:    October 2022
## 

# This script formats bathymetry data and extracts bathymetry derivatives for modelling habitat and fish
# As this raw bathymetry data is often too large for GitHub, the raw files are hidden in the .gitnore
# You have to download data and create this folder directory yourself for the script to run!

# Clear your environment
rm(list = ls())

# Load libraries - some more to add here
library(sp)
library(terra)
library(sf)
library(stars)
library(starsExtra)

# Set your study name
name <- "Parks-Ningaloo-synthesis"                                              # Change here

# This next section uses coarse GA bathymetry, replace if you have better bathymetry data (ie. multibeam or LiDAR)
bath_r      <- readRDS(paste(paste0('data/spatial/rasters/', name), 'ga_bathy.rds', sep = "_")) %>%
  rast(crs = "epsg:4326")
plot(bath_r)                                                                    # Plot to check everything looks ok

# Crop the bathymetry to the general study area
lats <- read.csv("data/tidy/Parks-Ningaloo-synthesis_random-points_broad.habitat.csv") %>%
  glimpse()

min(lats$latitude)
max(lats$latitude)
min(lats$longitude)
max(lats$longitude)

tbath_c <- crop(bath_r, ext(c(113.2, 114.3,-23, -21.5)))
plot(tbath_c)
points(lats[,c("longitude","latitude")], pch = 20, cex = 1, col = "red")
fbath_df <- as.data.frame(tbath_c, xy = TRUE)                                   # Convert this to a dataframe
# saveRDS(fbath_df, paste(paste0('data/spatial/rasters/',                         # Save it for use in the next scripts
#                                name), 'ga_bathy.rds', sep = "_")) 

# Calculate TERRA terrain derivatives
preds <- terrain(tbath_c, neighbors = 8,
                 v = c("slope", "aspect", "TPI", "TRI", "roughness"),           # Remove here as necessary
                 unit = "degrees")           
preds <- rast(list(tbath_c, preds))                                             # Stack the derivatives with the bathymetry

# Calculate detrended bathymetry
zstar <- st_as_stars(tbath_c)                                                   # Convert to a stars object
detre <- detrend(zstar, parallel = 8)                                           # Detrend bathymetry - This usually runs quite slow!
detre <- as(object = detre, Class = "SpatRaster")                               # Convert it to a raster
names(detre) <- c("detrended", "lineartrend")
preds <- rast(list(preds, detre))                                               # Make a rasterstack
plot(preds)

# Save the output
preds <- wrap(preds)
saveRDS(preds, paste(paste0('data/spatial/rasters/', name), 'ga_spatial_covariates.rds', sep = "_"))
