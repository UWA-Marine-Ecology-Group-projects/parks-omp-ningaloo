
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
# library(sf)
library(ggplot2)
library(reshape2)

sppcrs <- CRS("+proj=utm +zone=49 +south +datum=WGS84 +units=m +no_defs")
wgscrs <- CRS("+proj=longlat +datum=WGS84")

# get inclusion probs (from 'R/2_make_inclusionprobs.R')
inp_overall <- readRDS("output/2205_MBHDesign/ptcloates_inclusionp_rast.rds")
cellStats(inp_overall, "sum")

# not sure why but multiply inclusion p values so the sum equals 50?
inp_overall[] <- inp_overall[] * 46
plot(inp_overall)
cellStats(inp_overall, "sum")

# fix design parameters
nbruv <- 46
# bruv >350m apart - adding a few extra sites, manually remove any that are too near

## select sites
set.seed(42) # boss is using seed 40, 33. 42 was ok, 66, 70, 23 too close

tha_sites <- quasiSamp(n = nbruv, 
                       potential.sites = coordinates(inp_overall), 
                       inclusion.probs = values(inp_overall), 
                       nSampsToConsider = 10000)

tha_sites_sp <- SpatialPointsDataFrame(coords = cbind(tha_sites[1:2]), data = tha_sites)

## check spread of sites
# plot against inclusion probabilities
plot(inp_overall)
plot(tha_sites_sp, add = TRUE)

# bring in pref sites from previous survey - sites selected manually to preference seabed features
pref_ptc <- readOGR("output/planned/ptcloates_bruv_pref.shp")
pref_df  <- as.data.frame(pref_ptc, xy = TRUE)
head(pref_df)
pref_spdf <- SpatialPointsDataFrame(pref_ptc, data = pref_df)
proj4string(pref_spdf) <- wgscrs
pref_spdf <- spTransform(pref_spdf, sppcrs)

# get covariates
preds <- readRDS("output/2205_MBHDesign/ptc_covariate_rasts.rds")
site_covs <- cbind(tha_sites, extract(preds[[-7]], tha_sites_sp))
site_c_w  <- melt(site_covs, id.vars = 1:4)
ggplot(site_c_w, aes(ID, value)) + 
  geom_point(alpha = 3/5) +
  geom_smooth() +
  facet_wrap(~ variable, scales = "free")

# extract covariates for pref sites
pref_covs <- cbind(pref_df, extract(preds[[-7]], pref_spdf))
prefc_w   <- melt(pref_covs[-5], id.vars = 1:4)
prefc_w$source <- c("preferential")

preddf <- as.data.frame(preds[[-7]], na.rm = TRUE)
predw  <- melt(preddf)
predw$source <- c("rasters")
sitedat <- data.frame("variable" = site_c_w$variable, 
                      "value" = site_c_w$value, "source" = c("sites"))
alldat  <- rbind(predw, sitedat, prefc_w[5:7])

ggplot(alldat, aes(variable, value, colour = source)) + 
  geom_violin() + 
  facet_wrap(~ variable, scales = "free")

# convert sites to wgs84 and export
proj4string(tha_sites_sp) <- sppcrs
sites_wgs <- spTransform(tha_sites_sp, wgscrs)
sites_df  <- as.data.frame(sites_wgs, xy = TRUE)
head(sites_df)

# output to shapefile for field
colnames(sites_df) <- c("easting", "northing", "p_inclusion", 
                        "ID", "lon", "lat", "xy")
sites_df$sites     <- c("PC")
sites_df$site      <- c("PointCloates")
sites_df$methods   <- c("B")
sites_df$method    <- c("BRUV")
sites_df$pointnum  <- c(1:nrow(sites_df))
sites_df$dropcode  <- interaction(sites_df$methods, 
                                  sites_df$pointnum, sep = "")
sites_df <- merge(sites_df, site_covs, by = "ID")
sites_df$selected <- c("MBH")

sites_short <- sites_df[ , colnames(sites_df) %in% c("lon", "lat", "method", 
                                                     "dropcode", "selected")]
head(sites_short)

# tidy preferential site info and join
pref_df <- merge(pref_df, pref_covs, by = "id")

pref_df$method   <- c("BRUV")
pref_df$methods  <- c("B")
pref_df$pointnum <- c((nrow(sites_df) + 1):(nrow(sites_df) + nrow(pref_df)))
pref_df$dropcode <- interaction(pref_df$methods, pref_df$pointnum, sep = "")
colnames(pref_df)[3:4] <- c("lon", "lat")
pref_df$selected <- c("Preferential")
pref_short <- pref_df[ , colnames(pref_df) %in% c("lon", "lat", 
                                               "method", "dropcode", "selected")]
head(pref_short)
head(sites_short)
saveRDS(pref_short, "output/2205_MBHDesign/preferential_sitecodes.rds")

sites_short <- rbind(sites_short, pref_short)
head(sites_short)

write.csv(sites_short, 'output/2205_MBHDesign/planned/ptcloates_bruv_mbh.csv')

sites_sp <- SpatialPointsDataFrame(coords = sites_df[1:2], data = sites_df)
shapefile(sites_sp, "output/2205_MBHDesign/planned/ptcloates_bruv_mbh", overwrite = TRUE)

# save out a version with some of the covariates
sites_wcov <- sites_df[ , colnames(sites_df) %in% c("lon", "lat", 
                                                    "method", "dropcode", 
                                                    "selected", "depth", 
                                                    "slope", "p_inclusion")]
pref_df$p_inclusion <- c(NA)
pref_wcov <- pref_df[ , colnames(pref_df) %in% c("lon", "lat", 
                                                    "method", "dropcode", 
                                                    "selected", "depth", 
                                                    "slope", "p_inclusion")]
saveRDS(pref_wcov, "output/2205_MBHDesign/preferential_site_covs.rds")

sites_wcov <- rbind(sites_wcov, pref_wcov)
write.csv(sites_wcov, "output/2205_MBHDesign/planned/ptcloates_bruv_sites_wcovs.csv")
