
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
fbath  <- readRDS("output/ptcloates_50mbathy.rds")
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=49 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects

# clean data and sort out crs issues
aumpa <- aumpa[aumpa$ResName %in% c("Ningaloo"), ]
fbdf  <- as.data.frame(fbath, xy = TRUE)
colnames(fbdf)[3]   <- "Depth"
proj4string(aumpa)  <- wgscrs
aumpa               <- spTransform(aumpa, proj4string(fbath))

# define project area
ssite    <- newstrip(c(763800, 7491000), xdim = 3000, ydim = 15000, 
                     heading = 21, siteID = "SiteA", projcrs = sppcrs)
ssite_df <- fortify(ssite, xy = TRUE)

# plot site area
p1 <- ggplot() +
  geom_raster(data = fbdf, aes(x, y, fill = Depth)) +
  scale_fill_viridis(option = "D") +
  geom_contour(data = fbdf, aes(x = x, y = y, z = Depth),
  binwidth = 10, colour = "white", alpha = 3/5, size = 0.1) +
  geom_polygon(data = aumpa, aes(long, lat, group = group), alpha = 4/5) +
  geom_polygon(data = ssite_df, aes(long, lat),  fill = NA, colour = "red") +
  coord_sf(xlim = c(756000, 770000), ylim = c(7480000, 7500000)) +
  theme_minimal()
p1 

# prepare predictors
extent(ssite)
sa_ext <- extent(c(759600, 768000, 7483500, 7498500))                           # round the extent of ssite to nearest m
s_rast <- Blank.Raster(sa_ext, sppcrs, res(fbath))

# MP zone coding: 1 = Rec Use, 2 = NPZ
aumpa$inside <- c(1)
aumpa$inside[aumpa$ZoneName == "National Park Zone"] <- 2
mprast <- rasterize(x = aumpa, y = s_rast, field = 'inside') 

s_bath <- crop(fbath, sa_ext)
s_terr <- terrain(s_bath, neighbours = 8,
                  opt = c("slope", "aspect", "TPI", "TRI", "roughness"))

preds <- stack(s_bath, s_terr, mprast)
preds <- mask(preds, ssite)
names(preds)[c(1, 7)] <- c("depth", "mp_zone")
plot(preds)

saveRDS(preds, 'output/ptc_covariate_rasts.rds')

### SITE/SURVEY PLAN
## 48 BRUVs - 40 balanced design - 8 legacy/exciting spots/captain's pick
## BOSS to 300m - BRUV to 150m depth



