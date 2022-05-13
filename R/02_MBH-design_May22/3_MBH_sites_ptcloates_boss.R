
###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    MBH Design Site Selection - Pt Cloates Naked Boss
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
inp_overall <- readRDS("output/2205_MBHDesign/ptcloates_inclusionp_rast.rds")
cellStats(inp_overall, "sum")

# not sure why but multiply inclusion p values so the sum equals 50?
inp_overall[] <- inp_overall[] * 56
plot(inp_overall)
cellStats(inp_overall, "sum")

# fix design parameters
nboss <- 56
# boss - 100m w/ bait - 150m without

## select sites
set.seed(40)

tha_sites <- quasiSamp(n = nboss, 
                       potential.sites = coordinates(inp_overall), 
                       inclusion.probs = values(inp_overall), 
                       nSampsToConsider = 10000)

tha_sites_sp <- SpatialPointsDataFrame(coords = cbind(tha_sites[1:2]), data = tha_sites)

## check spread of sites
# plot against inclusion probabilities
plot(inp_overall)
plot(tha_sites_sp, add = TRUE)

# get covariates
preds <- readRDS("output/2205_MBHDesign/ptc_covariate_rasts.rds")

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

# output to shapefile for field
colnames(sites_df) <- c("easting", "northing", "p_inclusion", 
                        "ID", "lon", "lat", "xy")
sites_df$sites     <- c("PC")
sites_df$site      <- c("PointCloates")
sites_df$methods   <- c("N")
sites_df$method    <- c("Naked Boss")
sites_df$pointnum  <- c(201:(nrow(sites_df) + 200))
sites_df$dropcode  <- interaction(sites_df$methods, 
                                  sites_df$pointnum, sep = "")
sites_df <- merge(sites_df, site_covs, by = "ID")
sites_df$selected <- c("MBH")

sites_short <- sites_df[ , colnames(sites_df) %in% c("lon", "lat", "method", 
                                                     "dropcode", "selected")]
head(sites_short)

pref_short <- readRDS("output/2205_MBHDesign/preferential_sitecodes.rds")
pref_short$dropcode <- interaction(c("N"), c((201 + nrow(sites_df)):((200 + nrow(sites_df)) + nrow(pref_df))), sep = "")
pref_short$method <- c("Naked Boss")

sites_short <- rbind(sites_short, pref_short)
head(sites_short)

write.csv(sites_short, 'output/2205_MBHDesign/planned/ptcloates_nakedboss_mbh.csv')

sites_sp <- SpatialPointsDataFrame(coords = sites_df[5:6], data = sites_df)
shapefile(sites_sp, "output/2205_MBHDesign/planned/ptcloates_nakedboss_mbh", overwrite = TRUE)

# save out a version with some of the covariates
sites_wcov <- sites_df[ , colnames(sites_df) %in% c("lon", "lat", 
                                                    "method", "dropcode", 
                                                    "selected", "depth", 
                                                    "slope", "p_inclusion")]

pref_wcov <- readRDS("output/2205_MBHDesign/preferential_site_covs.rds")
pref_wcov$method <- c("Naked Boss")

sites_wcov <- rbind(sites_wcov, pref_wcov)
write.csv(sites_wcov, "output/2205_MBHDesign/planned/ptcloates_nakedboss_sites_wcovs.csv")


