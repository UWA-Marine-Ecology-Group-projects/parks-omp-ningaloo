
###
# Project: Parks - OMP Ningaloo August Survey
# Data:    Survey design covariates
# Task:    MBH Design Preparation - Pt Cloates
# author:  Kingsley Griffin
# date:    July/Aug 2021
##

library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(viridis)

source("R/functions.R")

# get and sort spatial boundaries
aumpa  <- readOGR("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
fbath  <- readRDS("output/nesp_5m_bathy_interp_ptcloates.rds")
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=49 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects

# clean data and sort out crs issues
aumpa <- aumpa[aumpa$ResName %in% c("Ningaloo"), ]
fbath[fbath[] < -150] <- NA
fbdf  <- as.data.frame(fbath, xy = TRUE)
colnames(fbdf)[3]   <- "Depth"
proj4string(aumpa)  <- wgscrs
aumpa               <- spTransform(aumpa, sppcrs)

## Pt Cloates ----
# define pt cloates project area
ptcsite    <- newstrip(c(761900, 7485000), xdim = 1200, ydim = 9000, 
                       heading = 13, siteID = "SiteA", projcrs = sppcrs)          # make a site box

plot(fbath)
plot(ptcsite, add=T)
plot(aumpa, add=T)
# ptc_df <- fortify(ptcsite, xy = TRUE)
# 
# # plot pt cloates site area
# p1 <- ggplot() +
#   geom_raster(data = fbdf, aes(x, y, fill = Depth)) +
#   scale_fill_viridis(option = "D") +
#   geom_contour(data = fbdf, aes(x = x, y = y, z = Depth),
#   binwidth = 10, colour = "white", alpha = 3/5, size = 0.1) +
#   geom_polygon(data = aumpa, aes(long, lat, group = group), alpha = 4/5) +
#   geom_polygon(data = ptc_df, aes(long, lat),  fill = NA, colour = "red") +
#   coord_equal(xlim = c(756000, 770000), ylim = c(7470000, 7500000)) +
#   theme_minimal()
# p1 

# prepare predictors 
extent(ptcsite)
ptc_ext  <- extent(c(760227.5, 763567.5, 7480487, 7489512))                      # the extent has to fit with the bathy extent. yes, it's annoying
ptc_rast <- Blank.Raster(ptc_ext, sppcrs, res(fbath))

# MP zone coding: 1 = Rec Use, 2 = NPZ
aumpa$inside <- c(1)
aumpa$inside[aumpa$ZoneName == "National Park Zone"] <- 2
mprast <- rasterize(x = aumpa, y = ptc_rast, field = 'inside') 

ptc_bath <- crop(fbath, ptc_ext)
ptc_terr <- terrain(ptc_bath, neighbours = 8, unit = "degrees",
                    opt = c("slope", "aspect", "TPI", "TRI", "roughness"))

preds <- stack(ptc_bath, ptc_terr, mprast)                                       # if extents don't match, refer to previous comment
preds <- mask(preds, ptcsite)
names(preds)[c(1, 7)] <- c("depth", "mp_zone")
plot(preds)

saveRDS(preds, 'output/ptc_covariate_rasts.rds')

## Yardie Creek -----
# define yardie project area
ysite <- newstrip(c(783000, 7533400), xdim = 2500, ydim = 10000, 
                       heading = 25, siteID = "SiteA", projcrs = sppcrs)
y_df  <- fortify(ysite, xy = TRUE)

# plot yardie site area
p1 <- ggplot() +
  geom_raster(data = fbdf, aes(x, y, fill = Depth)) +
  scale_fill_viridis(option = "D") +
  geom_contour(data = fbdf, aes(x = x, y = y, z = Depth),
               binwidth = 10, colour = "white", alpha = 3/5, size = 0.1) +
  geom_polygon(data = aumpa, aes(long, lat, group = group), alpha = 4/5) +
  geom_polygon(data = y_df, aes(long, lat),  fill = NA, colour = "red") +
  coord_equal(xlim = c(776000, 786000), ylim= c(7510000, 7540000)) +
  theme_minimal()
p1 

# prepare predictors 
extent(ysite)
y_ext  <- extent(c(779550, 786350, 7528350, 7538400))                           # round to nearest 50m/res of bathy
y_rast <- Blank.Raster(y_ext, sppcrs, res(fbath))

# MP zone coding: 1 = Rec Use, 2 = NPZ
mprast <- rasterize(x = aumpa, y = y_rast, field = 'inside') 

y_bath <- crop(fbath, y_ext)
y_terr <- terrain(y_bath, neighbours = 8, unit = "degrees",
                    opt = c("slope", "aspect", "TPI", "TRI", "roughness"))

preds <- stack(y_bath, y_terr, mprast)
preds <- mask(preds, ysite)
names(preds)[c(1, 7)] <- c("depth", "mp_zone")
plot(preds)

saveRDS(preds, 'output/yc_covariate_rasts.rds')


