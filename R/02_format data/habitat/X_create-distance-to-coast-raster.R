library(terra)
library(sf)
library(tidyverse)

rm(list = ls())

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%
  dplyr::filter(FEAT_CODE %in% "mainland")

st_crs(aus) <- gdacrs
aus <- st_transform(aus, wgscrs)
aus <- st_union(aus)                                                            # Joins the states together
ausout <- st_cast(aus, "MULTILINESTRING")                                       # 2 different polygon forms - this one not filled
plot(ausout)
ausv <- vect(ausout)
plot(ausv)

# Load bathy raster to calculate distance
bathy <- rast("data/spatial/rasters/raw bathymetry/bath_250_good.tif") %>%
  clamp(lower = -300, values = F) %>%
  crop(ext(100, 160, -50, -10)) %>%
  trim()

bathtest <- crop(bathy, ext(112, 115, -23, -21))
bathtest <- trim(bathtest)
plot(bathtest)

distcoast <- terra::distance(bathtest, ausv)
