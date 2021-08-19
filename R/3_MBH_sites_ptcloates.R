
###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    MBH Design Site Selection - Pt Cloates
# author:  Kingsley Griffin
# date:    Aug 2021
##

library(raster)
# install.packages("MBHdesign")
library(MBHdesign)
library(sp)
library(ggplot2)
library(reshape2)

# get inclusion probs (from 'R/2_make_inclusionprobs.R')
inp_overall <- readRDS("output/ptcloates_inclusionp_rast.rds")
cellStats(inp_overall, "sum")

# not sure why but multiply inclusion p values so the sum equals 50?
inp_overall[] <- inp_overall[] * 50
plot(inp_overall)
cellStats(inp_overall, "sum")

# fix design parameters
nbruv <- 30
# boss - 100m w/ bait - 150m without

## select sites
set.seed(42)

tha_sites <- quasiSamp(n = nbruv, 
                       potential.sites = coordinates(inp_overall), 
                       inclusion.probs = values(inp_overall), 
                       nSampsToConsider = 10000)

tha_sites_sp <- SpatialPointsDataFrame(coords = cbind(tha_sites[1:2]), data = tha_sites)

## check spread of sites
# plot against inclusion probabilities
plot(inp_overall)
plot(tha_sites_sp, add = TRUE)

# get covariates
preds <- readRDS("output/ptc_covariate_rasts.rds")

site_covs <- cbind(tha_sites, extract(preds[[-7]], tha_sites_sp))
site_c_w  <- melt(site_covs, id.vars = 1:4)
ggplot(site_c_w, aes(ID, value)) + 
  geom_point(alpha = 3/5) +
  geom_smooth() +
  facet_wrap(~ variable, scales = "free")

preddf <- as.data.frame(preds[[-7]], na.rm = TRUE)
predw  <- melt(preddf)
predw$source <- c("rasters")
sitedat <- data.frame("variable" = site_c_w$variable, "value" = site_c_w$value, "source" = c("sites"))
alldat  <- rbind(predw, sitedat)

ggplot(alldat, aes(variable, value, colour = source)) + 
  geom_violin() + 
  facet_wrap(~ variable, scales = "free")

# convert sites to wgs84 and export
sppcrs <- CRS("+proj=utm +zone=49 +south +datum=WGS84 +units=m +no_defs")
proj4string(tha_sites_sp) <- sppcrs
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sites_wgs <- spTransform(tha_sites_sp, wgscrs)
sites_df  <- as.data.frame(sites_wgs, xy = TRUE)

write.csv(sites_df, 'output/planned/ptcloates_bruv_mbh.csv')

