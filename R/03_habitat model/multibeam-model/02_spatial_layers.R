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
library(raster)
library(sf)
library(stars)
library(starsExtra)

# Set your study name
name <- "Parks-Ningaloo-synthesis"                                              # Change here

# Set CRS for bathymetry data
wgscrs <- "+proj=longlat +datum=WGS84 +south"                                   # Latlong projection 

# Read in the bathymetry
bathy <- rast("data/spatial/rasters/raw bathymetry/ptcloates_5m_WGS84_trimmed.tif") %>%
  trim() %>%
  clamp(upper = -20, values = F) # Weirdly put 0s in some spots instead of NAs
plot(bathy)
summary(bathy)

# Crop the bathymetry to the general study area
fbath_df <- as.data.frame(bathy, xy = TRUE, na.rm = T)                                   # Convert this to a dataframe
saveRDS(fbath_df, paste(paste0('data/spatial/rasters/',                         # Save it for use in the next scripts
                               name), 'nesp_bathy.rds', sep = "_")) 

# Calculate TERRA terrain derivatives
preds <- terrain(tbath_c, neighbors = 8,
                 v = c("slope", "aspect", "TPI", "TRI", "roughness"),           # Remove here as necessary
                 unit = "degrees")           
preds <- rast(list(tbath_c, preds))                                             # Stack the derivatives with the bathymetry

# Calculate detrended bathymetry
zstar <- st_as_stars(tbath_c)                                                   # Convert to a stars object
detre <- detrend(zstar, parallel = 8)                                           # Detrend bathymetry - This usually runs quite slow!
detre <- as(object = detre, Class = "SpatRaster")                                   # Convert it to a raster
names(detre) <- c("detrended", "lineartrend")
preds <- rast(list(preds, detre))                                               # Make a rasterstack
plot(preds)


# Save the output
preds <- wrap(preds)
saveRDS(preds, paste(paste0('data/spatial/rasters/', name), 'spatial_covariates.rds', sep = "_"))
