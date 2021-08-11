
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
nbruv <- 40

## select sites
set.seed(42)

tha_sites <- quasiSamp(n = nbruv, 
                       potential.sites = coordinates(inp_overall), 
                       inclusion.probs = values(inp_overall), 
                       nSampsToConsider = 5000)

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
ggplot(predw, aes(variable, value)) + 
  geom_violin() + 
  facet_wrap(~ variable, scales = "free_x")
sitedat <- data.frame("variable" = site_c_w$variable, "value" = site_c_w$value, "source" = c("sites"))
alldat <- rbind(predw, sitedat)

ggplot(alldat, aes(variable, value, colour = source)) + 
  geom_violin() + 
  facet_wrap(~ variable, scales = "free")
                