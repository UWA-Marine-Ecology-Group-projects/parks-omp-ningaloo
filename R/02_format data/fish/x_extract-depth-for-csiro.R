library(terra)
library(CheckEM)
library(sf)
library(tidyverse)

dat <- read.csv("data/raw/EM Export/2019-08_Ningaloo_stereo-BRUVs_Metadata.csv") %>%
  clean_names() %>%
  dplyr::select(sample, depth, longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>%
  st_transform(32749) %>%
  glimpse()

bathy <- rast("data/spatial/rasters/depth_195_50m.tif")
plot(bathy)
plot(dat, add = T)

tidy <- cbind(dat, terra::extract(bathy, dat)) %>%
  as.data.frame() %>%
  dplyr::select(-c(ID, geometry)) %>%
  dplyr::rename(depth_metadata = depth, depth_multibeam = depth_195_50m) %>%
  glimpse()

write.csv(tidy, file = "data/staging/2019-08_Ningaloo_stereo-BRUVs_Metadata-with-depth.csv")

test <- tidy %>%
  dplyr::filter(!depth_metadata %in% "?", !is.na(depth_multibeam)) %>%
  dplyr::mutate(depth_metadata = as.numeric(depth_metadata), depth_multibeam = abs(as.numeric(depth_multibeam))) %>%
  glimpse()

ggplot(data = test, aes(x = depth_multibeam, y = depth_metadata)) +
  geom_point()
