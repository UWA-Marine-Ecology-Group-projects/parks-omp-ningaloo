###
# Project: Parks OMP Ningaloo
# Data:    BRUVS, BOSS Habitat data
# Task:    Merging habitat data
# Author:  Kingsley Griffin & Claude Spencer
# Date:    October 2022
##

# This script takes formatted habitat data from TransectMeasure and joins it with bathymetry derivatives for modelling

# Clear your environment
rm(list = ls())

# Load libraries
library(reshape2)
library(dplyr)
library(terra)
library(sp)
library(ggplot2)

# Set your study name
name <- "Parks-Ningaloo-synthesis"                                              # Change here

# Load in tidy data from the formatting scripts
hab  <- read.csv("data/tidy/Parks-Ningaloo-synthesis_random-points_broad.habitat.csv") %>%
  glimpse()

# Extract bathy derivatives for modelling
# Set up CRS and load spatial covariates from 02_spatial_layers.R 
wgscrs <- "+proj=longlat +datum=WGS84 +south"                                    # Latlong projection 
preds  <- readRDS(paste(paste0('data/spatial/rasters/', name), 
                       'ga_spatial_covariates.rds', sep = "_"))
# preds <- rast(preds)
plot(preds)

# Align crs and check samples over bathy and extract terrain data
allhab_sp <- vect(hab, geom = c("longitude", "latitude"), crs = wgscrs)         # Convert the habitat data to a terra vector
plot(preds[[1]])                                                                # Plot the first bathymetry derivative
plot(allhab_sp, add = T)                                                        # Add the sampling points to check if they align
habt_df   <- as.data.frame(allhab_sp, geom = "XY")                              # Convert the habitat data back to a regular dataframe
habi_df   <- cbind(habt_df, terra::extract(preds, allhab_sp))                   # Extract the bathymetry derivatives and join on as a dataframe

# Rename columns and combine habitat columns for modelling
# Change this for your project needs!!
allhab <- habi_df %>%
  dplyr::rename(sand = broad.unconsolidated) %>%
  dplyr::mutate(inverts = broad.hydrocoral + broad.sponges + broad.octocoral.black +               # Make a sessile invertebrate column
                  broad.invertebrate.complex +  broad.hydroids + 
                  broad.bryozoa + broad.crinoids) %>%
  glimpse()                                                                     # Preview data

# Save the output
saveRDS(allhab, paste(paste0('data/tidy/', name), 
                      'ga_habitat-bathy-derivatives.rds', sep = "_"))
