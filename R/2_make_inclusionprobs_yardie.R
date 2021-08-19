
###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    MBH Design Preparation - Yardie
# author:  Kingsley Griffin
# date:    Aug 2021
##

library(raster)

# read in covariate rasters
preds <- readRDS('output/yc_covariate_rasts.rds')
plot(preds)
preddf <- as.data.frame(preds, xy = TRUE)
preddf <- preddf[is.na(preddf$slope) != TRUE, ]
preddf[is.na(preddf$mp_zone) == TRUE, ] <- 0
summary(preddf)

### SET BASIC SITE/SURVEY PLAN
## 40 BRUVs - 30 balanced design - 10 legacy/exciting spots/captain's pick
## MAX DEPTHs: BOSS to 300m - BRUV to 150m depth - this shouldn't be an issue
nbruv <- 16

## Build in design shape (stratification biases)
# convert covariates to categorical layers with breaks that define areas of interest, or evenly using quantiles
# then define stratification bias by weighting the number of sampling sites that will ideally fall within each category
hist(preds$depth, breaks = 50)
bathy_qs   <- c(0, 0.2, 0.4, 0.6, 0.8, 1)                                                 # mainly interested in middle band around old coast
bathy_cuts <- quantile(preds$depth, bathy_qs)
bathy_cuts
cat_bathy  <- cut(preds$depth, breaks = bathy_cuts, na.rm = TRUE)
plot(stack(preds$depth, cat_bathy))                                             # compare categorical with original data
bathy_split <- data.frame(zones = unique(cat_bathy),
                          split = c(0.1, 0.2, 0.5, 0.1, 0.1))                             # split sampling among zones
bathy_split$zbruv <- bathy_split$split * nbruv                                  # n samples in each zone
bathy_split

hist(preds$slope)
slope_qs   <- c(0, 0.7, 0.9, 1)
slope_cuts <- quantile(preds$slope, slope_qs)
slope_cuts
cat_slope  <- cut(preds$slope, breaks = slope_cuts, na.rm = TRUE)
plot(stack(preds$slope, cat_slope))
slope_split <- data.frame(zones = unique(cat_slope),
                          split = c(0.1, 0.4, 0.5))
slope_split$zbruv <- round(slope_split$split * nbruv)
slope_split

mp_zone_split <- data.frame(zones = unique(preds$mp_zone),
                          split = c(0.5, 0.5))
mp_zone_split$zbruv <- mp_zone_split$split * nbruv
mp_zone_split

# create inclusion probability rasters for levels of each covariate
# choose carefully here - if you can make do with less rasters for selection, do that
ic_rasts        <- stack(cat_bathy, cat_slope, preds$mp_zone)
names(ic_rasts) <- c("cat_bathy", "cat_slope", "mp_zone")
plot(ic_rasts)

icr_df <- as.data.frame(ic_rasts, xy = TRUE)
inp_rasts <- ic_rasts

# calculate inclusion probabilities for each variable
bath_lvl_sum  <- table(icr_df$cat_bathy)                                       # count of cells of each level
bath_p_strata <- bath_lvl_sum / sum(bath_lvl_sum)                              # proportion of raster covered by each level
bath_inclp    <- bathy_split$split / bath_p_strata                             # alter inclusion prob based on target sample numbers

slope_lvl_sum  <- table(icr_df$cat_slope)
slope_p_strata <- slope_lvl_sum / sum(slope_lvl_sum)
slope_inclp    <- slope_split$split / slope_p_strata

mp_zone_lvl_sum  <- table(icr_df$mp_zone)
mp_zone_p_strata <- mp_zone_lvl_sum / sum(mp_zone_lvl_sum)
mp_zone_inclp    <- mp_zone_split$split / mp_zone_p_strata

# translate this onto inclusion probability rasters for each layer - leaving this in long format for easy interp
for(lev in 1:length(bath_inclp)){
  inp_rasts$cat_bathy[inp_rasts$cat_bathy == lev] <- bath_inclp[lev]
}

for(lev in 1:length(slope_inclp)){
  inp_rasts$cat_slope[inp_rasts$cat_slope == lev] <- slope_inclp[lev]
}

for(lev in 1:length(mp_zone_inclp)){
  inp_rasts$mp_zone[inp_rasts$mp_zone == lev] <- mp_zone_inclp[lev]
}

# scale the layers so there is equal influence of each
for(rasti in 1:nlayers(inp_rasts)){
  inp_rasts[[rasti]] <- inp_rasts[[rasti]] / sum(rasti[], na.rm = TRUE)
}

plot(inp_rasts)
inp_overall <- sum(inp_rasts)
plot(inp_overall)
cellStats(inp_overall, 'sum') # can't remember why we check this?
inp_overall[] <- inp_overall[] / sum(inp_overall[], na.rm = TRUE)
cellStats(inp_overall, 'sum') # to make it equal 1?

saveRDS(inp_overall, "output/yardie_inclusionp_rast.rds")

