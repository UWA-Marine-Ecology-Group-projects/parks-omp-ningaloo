###
# Project: Parks OMP Ningaloo
# Data:    BRUV and BOSS habitat data
# Task:    Generate full species table with CAAB codes
# Author:  Claude Spencer
# Date:    April 2023
##

# Clear environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(readxl)
library(googlesheets4)
library(GlobalArchive)

name <- "Parks-Ningaloo-synthesis"

# Load data
caab <- read.csv("data/staging/benthic-annotation-schema-forward-facing - code crosswalk.csv") %>%
  dplyr::select(CAAB_code, level_2, level_3, level_4) %>%
  dplyr::rename(broad = level_2, morphology = level_3, type = level_4) %>%
  glimpse()

# Honestly easier for now to add these in manually

detailed.points <- read.csv(paste0("data/staging/", name, "_random-points_detailed.habitat.csv")) %>%
  glimpse()

dat <- data.frame(classes = names(detailed.points[,3:29])) %>%
  dplyr::mutate(classes = str_to_title(str_replace_all(classes, "\\.", " "))) %>%
  separate(classes, into = c(NA,"broad", "morphology", "type"),
           extra = "merge") %>%
  dplyr::mutate(morphology = ifelse(type %in% "Cnidaria Matrix ", "Cnidaria Matrix ", morphology),
                type = ifelse(type %in% "Cnidaria Matrix ", "", type))

write.csv(dat, file = paste0("data/tidy/", name, "_habitat-list.csv"),
          row.names = F)

