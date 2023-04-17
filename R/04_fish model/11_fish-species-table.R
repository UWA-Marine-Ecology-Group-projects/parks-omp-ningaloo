###
# Project: Parks OMP Ningaloo
# Data:    BRUV and BOSS fish MaxN and length data
# Task:    Format data for use in FSSgam model selection
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

# Load fish data
dat <- read.csv(paste0("data/tidy/", name, ".checked.maxn.csv")) %>%
  glimpse()

unique(dat$species)

url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url) %>%
  ga.clean.names() %>%
  filter(grepl('Australia', global.region)) %>% # Change country here
  dplyr::select(family,genus,species,australian.common.name, caab) %>%
  distinct() %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::select(scientific, australian.common.name, caab) %>%
  glimpse()

1

caabdump <- read_excel("data/staging/caab_dump_latest.xlsx") %>%
  dplyr::filter(SCIENTIFIC_NAME %in% paste(GENUS, "spp.", sep = " "),
                between(SPCODE, 36000000, 38000000)) %>%
  dplyr::select(GENUS, SPCODE) %>%
  ga.clean.names() %>%
  distinct() %>%
  dplyr::filter(!is.na(genus)) %>%
  glimpse()

test <- caabdump %>%
  group_by(genus) %>%
  dplyr::summarise(n = n())

species <- dat %>%
  dplyr::filter(!species %in% c("sus"),
                !genus %in% c("NA"),
                !is.na(genus),
                !is.na(species)) %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::select(family, genus, species, scientific) %>%
  distinct() %>%
  left_join(master) %>%
  left_join(caabdump, by = "genus") %>%
  dplyr::mutate(caab = ifelse(is.na(caab), spcode, caab)) %>%
  arrange(scientific) %>%
  dplyr::select() %>%
  dplyr::rename("Scientific name" = scientific,
                "Common name" = australian.common.name,
                "CAAB code" = caab) %>%
  glimpse()


write.csv(species, file = paste0("data/tidy/", name, "_species-list.csv"),
          row.names = F)

# Then manually update common names for spp

