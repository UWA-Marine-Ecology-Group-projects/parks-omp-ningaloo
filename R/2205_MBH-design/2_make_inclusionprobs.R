
###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    MBH Design Preparation - Pt Cloates
# author:  Kingsley Griffin
# date:    Aug 2021
##

library(raster)

# read in covariate rasters
preds <- readRDS('output/2205_MBHDesign/ptc_covariate_rasts.rds')
plot(preds)
preddf <- as.data.frame(preds, xy = TRUE)
preddf <- preddf[is.na(preddf$slope) != TRUE, ]
preddf[is.na(preddf$mp_zone) == TRUE, ] <- 0
summary(preddf)

### SET BASIC SITE/SURVEY PLAN
## 40 BRUVs - 30 balanced design - 10 legacy/exciting spots/captain's pick
## MAX DEPTHs: BOSS to 300m - BRUV to 150m depth - this shouldn't be an issue
nbruv <- 40

## Build in design shape (stratification biases)
# convert covariates to categorical layers with breaks that define areas of interest, or evenly using quantiles
# then define stratification bias by weighting the number of sampling sites that will ideally fall within each category
hist(preds$depth, breaks = 50)
bathy_qs   <- c(0, 0.2, 0.35, 0.6, 0.8, 1)                                                 # mainly interested in middle band around old coast
bathy_cuts <- quantile(preds$depth, bathy_qs)
bathy_cuts
cat_bathy  <- cut(preds$depth, breaks = bathy_cuts, na.rm = TRUE)
plot(stack(preds$depth, cat_bathy))                                             # compare categorical with original data
bathy_split <- data.frame(zones = unique(cat_bathy),
                          split = c(0.1, 0.2, 0.1, 0.5, 0.1))                             # split sampling among zones
bathy_split$zbruv <- bathy_split$split * nbruv                                  # n samples in each zone
bathy_split

hist(preds$aspect)
aspect_qs   <- c(0, 0.01, 0.04, 1)
aspect_cuts <- quantile(preds$aspect, aspect_qs)
aspect_cuts
cat_aspect  <- cut(preds$aspect, breaks = aspect_cuts, na.rm = TRUE)
plot(stack(preds$aspect, cat_aspect))
aspect_split <- data.frame(zones = unique(cat_aspect),
                           split = c(0.1, 0.2, 0.7))
aspect_split$zbruv <- aspect_split$split * nbruv
aspect_split

hist(preds$slope)
slope_qs   <- c(0, 0.7, 0.95, 1)
slope_cuts <- quantile(preds$slope, slope_qs)
slope_cuts
cat_slope  <- cut(preds$slope, breaks = slope_cuts, na.rm = TRUE)
plot(stack(preds$slope, cat_slope))
slope_split <- data.frame(zones = unique(cat_slope),
                          split = c(0.1, 0.4, 0.5))
slope_split$zbruv <- slope_split$split * nbruv
slope_split

hist(preds$tri)
tri_qs   <- c(0, 0.8, 0.95, 1)
tri_cuts <- quantile(preds$tri, tri_qs)
cat_tri  <- cut(preds$tri, breaks = tri_cuts, na.rm = TRUE)
plot(stack(preds$tri, cat_tri))
tri_split <- data.frame(zones = unique(cat_tri),
                        split = c(0.2, 0.4, 0.4))
tri_split$zbruv <- tri_split$split * nbruv
tri_split

hist(preds$tpi)
tpi_qs   <- c(0, 0.1, 0.9, 1)
tpi_cuts <- quantile(preds$tpi, tpi_qs)
cat_tpi  <- cut(preds$tpi, breaks = tpi_cuts, na.rm = TRUE)
plot(stack(preds$tpi, cat_tpi))
tpi_split <- data.frame(zones = unique(cat_tpi),
                        split = c(0.2, 0.2, 0.6))
tpi_split$zbruv <- tpi_split$split * nbruv
tpi_split

hist(preds$roughness)
roughness_qs   <- c(0, 0.6, 0.85, 0.95, 1)
roughness_cuts <- quantile(preds$roughness, roughness_qs)
cat_roughness  <- cut(preds$roughness, breaks = roughness_cuts, na.rm = TRUE)
plot(stack(preds$roughness, cat_roughness))
rough_split <- data.frame(zones = unique(cat_roughness),
                          split = c(0.1, 0.2, 0.3, 0.4))
rough_split$zbruv <- rough_split$split * nbruv
rough_split

# create inclusion probability rasters for levels of each covariate
# choose carefully here - if you can make do with less rasters for selection, do that
ic_rasts        <- stack(cat_bathy, cat_slope)
names(ic_rasts) <- c("cat_bathy", "cat_slope")
plot(ic_rasts)

icr_df <- as.data.frame(ic_rasts, xy = TRUE)
inp_rasts <- ic_rasts

# calculate inclusion probabilities for each variable
bath_lvl_sum  <- table(icr_df$cat_bathy)                                       # count of cells of each level
bath_p_strata <- bath_lvl_sum / sum(bath_lvl_sum)                              # proportion of raster covered by each level
bath_inclp    <- bathy_split$split / bath_p_strata                             # alter inclusion prob based on target sample numbers

aspect_lvl_sum  <- table(icr_df$cat_aspect)
aspect_p_strata <- aspect_lvl_sum / sum(aspect_lvl_sum)
aspect_inclp    <- aspect_split$split / aspect_p_strata

slope_lvl_sum  <- table(icr_df$cat_slope)
slope_p_strata <- slope_lvl_sum / sum(slope_lvl_sum)
slope_inclp    <- slope_split$split / slope_p_strata

tri_lvl_sum  <- table(icr_df$cat_tri)
tri_p_strata <- tri_lvl_sum / sum(tri_lvl_sum)
tri_inclp    <- tri_split$split / tri_p_strata

tpi_lvl_sum  <- table(icr_df$cat_tpi)
tpi_p_strata <- tpi_lvl_sum / sum(tpi_lvl_sum)
tpi_inclp    <- tpi_split$split / tpi_p_strata

rough_lvl_sum  <- table(icr_df$cat_rough)
rough_p_strata <- rough_lvl_sum / sum(rough_lvl_sum)
rough_inclp    <- rough_split$split / rough_p_strata


# translate this onto inclusion probability rasters for each layer - leaving this in long format for easy interp
for(lev in 1:length(bath_inclp)){
  inp_rasts$cat_bathy[inp_rasts$cat_bathy == lev] <- bath_inclp[lev]
}

for(lev in 1:length(aspect_inclp)){
  inp_rasts$cat_aspect[inp_rasts$cat_aspect == lev] <- aspect_inclp[lev]
}

for(lev in 1:length(slope_inclp)){
  inp_rasts$cat_slope[inp_rasts$cat_slope == lev] <- slope_inclp[lev]
}

for(lev in 1:length(tri_inclp)){
  inp_rasts$cat_tri[inp_rasts$cat_tri == lev] <- tri_inclp[lev]
}

for(lev in 1:length(tpi_inclp)){
  inp_rasts$cat_tpi[inp_rasts$cat_tpi == lev] <- tpi_inclp[lev]
}

for(lev in 1:length(rough_inclp)){
  inp_rasts$cat_rough[inp_rasts$cat_rough == lev] <- rough_inclp[lev]
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


saveRDS(inp_overall, "output/2205_MBHDesign/ptcloates_inclusionp_rast.rds")

writeRaster(inp_overall, "output/2205_MBHDesign/ptcloates_inclusionp_rast.tif")

