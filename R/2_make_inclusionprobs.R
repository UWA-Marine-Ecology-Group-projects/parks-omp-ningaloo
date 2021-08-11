
###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    MBH Design Preparation - Pt Cloates
# author:  Kingsley Griffin
# date:    Aug 2021
##

library(raster)

# read in covariate rasters
preds <- readRDS('output/ptc_covariate_rasts.rds')
plot(preds)
preddf <- as.data.frame(preds, xy = TRUE)
preddf <- preddf[is.na(preddf$slope) != TRUE, ]
preddf[is.na(preddf$mp_zone) == TRUE, ] <- 0
summary(preddf)

### SET BASIC SITE/SURVEY PLAN
## 48 BRUVs - 40 balanced design - 8 legacy/exciting spots/captain's pick
## MAX DEPTHs: BOSS to 300m - BRUV to 150m depth - this shouldn't be an issue
nbruv <- 40

## Build in design shape (stratification biases)

# Convert covariates to categorical layers focusing on key areas, via quantile cut points
hist(preds$depth)
bathy_qs   <- c(0, 0.3, 0.7, 1)                                                 # use a wider central zone to generate higher probability zone around main features
bathy_cuts <- quantile(preds$depth, bathy_qs)
cat_bathy  <- raster::cut(preds$depth, breaks = bathy_cuts, na.rm = TRUE)
plot(stack(preds$depth, cat_bathy))                                             # compare

hist(preds$aspect)
aspect_qs   <- c(0, 0.05, 0.5, 1)                                               # using quantiles to define the area we expect is old coastline
aspect_cuts <- quantile(preds$aspect, aspect_qs)
cat_aspect  <- raster::cut(preds$aspect, breaks = aspect_cuts, na.rm = TRUE)
plot(stack(preds$aspect, cat_aspect))

hist(preds$slope)
slope_qs   <- c(0, 0.3, 0.8, 1)                                                 # using quantiles to define the area we expect is old coast
slope_cuts <- quantile(preds$slope, slope_qs)
cat_slope  <- raster::cut(preds$slope, breaks = slope_cuts, na.rm = TRUE)
plot(stack(preds$slope, cat_slope))

# define weighting of sites in each category
zone_split <- data.frame(zones= c("NPZ", "REC", "Open"), 
                         zcode = c(2, 1, 0),
                         split = c(0.5, 0.1, 0.4))                              # split between NPZ, Rec Use, and Open
zone_split$zbruv <- zone_split$split * nbruv
zone_split

bathy_split <- data.frame(zones = unique(cat_bathy),
                          split = c(0.2, 0.5, 0.3))                             # bias sampling into central range of bathy
bathy_split$zbruv <- bathy_split$split * nbruv                                  # n samples in each zone
bathy_split

aspect_split <- data.frame(zones = unique(cat_aspect),
                          split = c(0.5, 0.25, 0.25))                           # bias sampling into area we expect is old coast
aspect_split$zbruv <- aspect_split$split * nbruv                                # n samples in each zone
aspect_split

slope_split <- data.frame(zones = unique(cat_slope),
                           split = c(0.3, 0.3, 0.4))                            # bias sampling into area we expect is old coast
slope_split$zbruv <- slope_split$split * nbruv                                  # n samples in each zone
slope_split

# create inclusion probability rasters for levels of each covariate
ic_rasts        <- stack(preds$mp_zone, cat_bathy, cat_aspect, cat_slope)
names(ic_rasts) <- c("mp_zone", "cat_bathy", "cat_aspect", "cat_slope")
plot(ic_rasts)

icr_df <- as.data.frame(ic_rasts, xy = TRUE)
inp_rasts <- ic_rasts

# calculate inclusion probabilities for each variable
bath_lvl_sum   <- table(icr_df$cat_bathy)                                       # count of cells of each level
bath_p_strata  <- bath_lvl_sum / sum(bath_lvl_sum)                              # convert to proportion
bath_inclp     <- bathy_split$split / bath_p_strata                             # inclusion prob for each level based on target sample

aspect_lvl_sum   <- table(icr_df$cat_aspect)                                    # count of cells of each level
aspect_p_strata  <- aspect_lvl_sum / sum(aspect_lvl_sum)                        # convert to proportion
aspect_inclp     <- aspect_split$split / aspect_p_strata                        # inclusion prob for each level based on target sample

slope_lvl_sum   <- table(icr_df$cat_slope)                                      # count of cells of each level
slope_p_strata  <- slope_lvl_sum / sum(slope_lvl_sum)                           # convert to proportion
slope_inclp     <- slope_split$split / slope_p_strata                           # inclusion prob for each level based on target sample


# translate this onto inclusion probability rasters for each layer
for(lev in 1:length(bath_inclp)){
  inp_rasts$cat_bathy[inp_rasts$cat_bathy == lev] <- bath_inclp[lev]
}

for(lev in 1:length(aspect_inclp)){
  inp_rasts$cat_aspect[inp_rasts$cat_aspect == lev] <- aspect_inclp[lev]
}

for(lev in 1:length(slope_inclp)){
  inp_rasts$cat_slope[inp_rasts$cat_slope == lev] <- slope_inclp[lev]
}

# scale the layers so there is equal influence of each
for(rasti in 1:nlayers(inp_rasts)){
  inp_rasts[[rasti]] <- inp_rasts[[rasti]] / sum(rasteri[], na.rm = TRUE)
}

plot(inp_rasts[[-1]])
inp_overall <- sum(inp_rasts[[-1]])
plot(inp_overall)
cellStats(inp_overall, 'sum') # can't remember why we check this?
inp_overall[] <- inp_overall[] / sum(inp_overall[], na.rm = TRUE)
cellStats(inp_overall, 'sum') # to make it equal 1?


saveRDS(inp_overall, "output/ptcloates_inclusionp_rast.rds")

