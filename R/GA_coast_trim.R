
###
# Project: Parks - Ningaloo Post-Survey
# Data:    GA Coarse Bathy/elevation data
# Task:    Trim down huge GA raster
# author:  Kingsley Griffin
# date:    Jul 2021
##

library(raster)
library(sf)
library(fields)

# download relevant tiles from: https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
# read in and merge GA coarse bathy tiles
cbaths <- list.files("data/spatial/raster/SS", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, 
                 function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 5 & cbathy$X < 115 & cbathy$X > 112 &
                   cbathy$Y > -23.5 & cbathy$Y < -21, ]                             # remove elevation and crop to w.coast
bath_r <- rasterFromXYZ(cbathy)
plot(bath_r)

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
ptc_ex <- extent(750000, 790000, 7450000, 7550000)
ffbath <- crop(ffbath, ptc_ex)
plot(ffbath)
saveRDS(ffbath, 'output/ptcloates_50mbathy.rds')

# 5m bathy layer - bring in and merge relevant panes
# pt cloates panes
# fbatha <- raster("data/spatial/raster/ningaloo_nesp_all_5m.120.tif")
fbathb <- raster("data/spatial/raster/ningaloo_nesp_all_5m.130.tif")
# fbathc <- raster("data/spatial/raster/ningaloo_nesp_all_5m.140.tif")
# fbaths <- merge(fbatha, fbathb)
plot(fbathb) # just work here for now (hopefully)

# interpolate to fill holes in the 5m data using TPS - be aware that this step is very slow

fbathten <- aggregate(fbathb, 20)
plot(fbathten)
fbath_df <- as.data.frame(fbathten, xy = TRUE, na.rm = TRUE)
fb_tps   <- fastTps(x = cbind(fbath_df$x, fbath_df$y), 
                    Y = fbath_df$ningaloo_nesp_all_5m.130) # if this creates error, may have to use Tps (slow)
bathi    <- interpolate(fbathb, fb_tps)
plot(bathi)
bathy_orig <- mask(bathi, fbathb, inverse = TRUE) # select non-holes
bathy_merg <- merge(bathi, bathy_orig)
plot(bathy_merg)

saveRDS(bathy_merg, "output/nesp_5m_bathy_interp_ptcloates.rds")

# Yardie Ck area

# pt cloates panes
ybatha <- raster("data/spatial/raster/ningaloo_nesp_all_5m.82.tif")
ybathb <- raster("data/spatial/raster/ningaloo_nesp_all_5m.92.tif")
ybaths <- merge(ybatha, ybathb)
plot(ybaths) # just work here for now (hopefully)

# interpolate to fill holes in the 5m data using TPS - be aware that this step is very slow
ybathag <- aggregate(ybaths, 20)
# yag_df  <- as.data.frame(ybathag, xy = TRUE, na.rm = TRUE)
# yc_tps  <- Tps(x = yag_df[1:2], Y = yag_df[3])     # hashing this in favour of aggregated vals
# yint    <- interpolate(ybaths, fb_tps)
# plot(yint)
ybathdag <- disaggregate(ybathag, 20)
y_orig <- mask(ybathdag, ybaths, inverse = TRUE) # select hole contents from 20m aggregated raster
y_merg <- merge(ybaths, y_orig)
plot(y_merg)

saveRDS(y_merg, "output/nesp_5m_bathy_interp_yardie.rds")

# also trim down coastal waters line

cwatr <- st_read("data/spatial/shp/amb_coastal_waters_limit.shp")               # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 110, xmax = 117, ymin = -26, ymax = -19))      # crop down the coastal waters line to general project area
saveRDS(cwatr, 'output/coastal_waters_limit_trimmed.rds')
