###
# Project: Parks OMP Ningaloo
# Data:    BRUVS, BOSS Habitat data
# Task:    Model habitat
# Author:  Kingsley Griffin & Claude Spencer
# Date:    October 2022
##

# This script takes formatted habitat data from TransectMeasure and joins it with bathymetry derivatives for modelling

# Clear your environment
rm(list = ls())

# Load libraries
library(randomForest)
library(reshape2)
library(dplyr)
library(terra)
library(sf)
library(ggplot2)

# Set your study name
name <- "Parks-Ningaloo-synthesis"                                              # Change here

# Load data
dat <- readRDS(paste(paste0('data/tidy/', name), 
                      'habitat-bathy-derivatives.rds', sep = "_")) %>%
  glimpse()
