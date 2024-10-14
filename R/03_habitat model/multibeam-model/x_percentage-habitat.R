rm(list = ls())

library(tidyverse)

broadbath <- readRDS("data/spatial/rasters/Parks-Ningaloo-synthesis_ga_spatial_covariates.rds")
broadbath_t <- project(broadbath, "epsg:32749")  
plot(broadbath_t)

dat <- readRDS("data/tidy/Parks-Ningaloo-synthesis_nesp-habitat-bathy-derivatives.rds") %>%
  dplyr::select(campaignid, sample, x, y, broad.consolidated, sand, inverts) %>%
  st_as_sf(coords = c("x", "y"), crs = 32749) %>%
  glimpse()

table <- dat %>%
  pivot_longer(cols = c("broad.consolidated", "sand", "inverts"), 
               names_to = "habitat", values_to = "count") %>%
  dplyr::group_by(habitat) %>%
  dplyr::summarise(count = sum(count)) %>%
  add_row(habitat = "sum", count = sum(.$count)) %>%
  glimpse()

# Rock
(29/7278) * 100
# Sand
(5925/7278) * 100
# Inverts
(1324/7278) * 100

dat_rari <- cbind(dat, terra::extract(broadbath_t, dat)) %>%
  dplyr::mutate(edc = case_when(between(Z, -200, -70) ~ "rariphotic",
                                between(Z, -70, -30) ~ "mesophotic",
                                between(Z, -250, -200) ~ "deep")) %>%
  dplyr::group_by(edc) %>%
  summarise(inverts = sum(inverts)) %>%
  add_row(edc = "sum", inverts = sum(.$inverts)) %>%
  glimpse()

# % in rariphotic
(1699/1740) * 100
