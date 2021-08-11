
###
# Project: Parks - Ningaloo Post-Survey
# Data:    GA Coarse Bathy/elevation data
# Task:    Trim down huge GA raster
# author:  Kingsley Griffin
# date:    Jul 2021
##

library(raster)
library(sf)

# download relevant tiles from: https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
# read in and merge GA coarse bathy tiles
cbaths <- list.files("data/spatial/raster", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, 
                 function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 5 & cbathy$X < 117, ]                             # remove elevation and crop to w.coast
bath_r <- rasterFromXYZ(cbathy)
# plot(bath_r)

# aggregate raster to reduce size and plotting time etc
aggbath  <- aggregate(bath_r, 10, fun = max, na.rm = TRUE)
plot(aggbath)
abath_df <- as.data.frame(aggbath, xy = TRUE)

saveRDS(abath_df, 'output/ga_bathy_trim.rds')

# fine bathy near survey area
fbath    <- crop(bath_r, extent(c(113.2, 114.5, -23, -21.5)))
fbath_df <- as.data.frame(fbath, xy = TRUE)

saveRDS(fbath_df, 'output/ga_bathy_fine.rds')

rm(cbathy, bath_r, aggbath, abath_df, fbath)

# 50m bathy layer - trim to pt cloates project area

ffbath <- raster("data/spatial/raster/depth_195_50m.tif")
ffbath <- flip(ffbath, direction = "y")
ptc_ex <- extent(750000, 780000, 7450000, 7510000)
ffbath <- crop(ffbath, ptc_ex)
plot(ffbath)
saveRDS(ffbath, 'output/ptcloates_50mbathy.rds')

# also trim down coastal waters line

cwatr <- st_read("data/spatial/shp/amb_coastal_waters_limit.shp")               # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 110, xmax = 117, ymin = -26, ymax = -19))      # crop down the coastal waters line to general project area
saveRDS(cwatr, 'output/coastal_waters_limit_trimmed.rds')
