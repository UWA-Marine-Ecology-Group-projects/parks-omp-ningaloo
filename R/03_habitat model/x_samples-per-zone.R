rm(list = ls())

library(tidyverse)
library(sf)

dat <- read.csv("data/tidy/Parks-Ningaloo-synthesis_random-points_broad.habitat.csv") %>%
  dplyr::select(campaignid, sample, longitude, latitude) %>%
  dplyr::mutate(method = case_when(str_detect(campaignid, "BRUV") ~ "BRUV",
                                   str_detect(campaignid, "BOSS") ~ "BOSS"),
                year = str_extract(campaignid, "^\\d{4}")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  glimpse()

zones <- st_read("data/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ningaloo", "Gascoyne")) %>%
  st_transform(4326) %>%
  glimpse()

ggplot() +
  geom_sf(data = zones) +
  geom_sf(data = dat)

combined <- st_intersection(dat, zones)

table <- combined %>%
  dplyr::group_by(campaignid, method, year, zone, epbc) %>%
  dplyr::summarise(n = n())
