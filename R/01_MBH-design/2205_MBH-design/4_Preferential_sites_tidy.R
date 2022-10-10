

###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    Tidying preferential sites
# author:  Kingsley Griffin
# date:    Aug 2021
##


# Pt Cloates - sites selected manually to preference seabed features manually
pref_ptc <- readOGR("output/planned/ptcloates_bruv_pref.shp")
head(pref_ptc)

sites_df <- as.data.frame(pref_ptc, xy= TRUE)
head(sites_df)

# output to shapefile for field
colnames(sites_df) <- c("ID", "feature", "lon", "lat", "xy")
sites_df$sites     <- c("PC")
sites_df$site      <- c("point_cloates")
sites_df$methods   <- c("PREF")
sites_df$method    <- c("PREF")
sites_df$pointnum  <- c(1:nrow(sites_df))
sites_df$dropcode  <- interaction(sites_df$sites, sites_df$methods, 
                                  sites_df$pointnum, sep = "")
sites_df <- sites_df[ , colnames(sites_df) %in% 
                        c("lon", "lat", "dropcode", "site", 
                          "method", "pointnum", "feature")]
sites_df$selected <- c("PREF")
head(sites_df)

sites_spdf <- SpatialPointsDataFrame(sites_df[2:3], data = sites_df)
plot(sites_spdf)

shapefile(sites_spdf, "output/planned/ptcloates_pref", overwrite = TRUE)

# 
# # Yardie - 
# pref_y <- readOGR("output/planned/yardie_bruv_pref.shp")
# head(pref_y)
# 
# sites_df <- as.data.frame(pref_y, xy= TRUE)
# head(sites_df)
# 
# # output to shapefile for field
# colnames(sites_df) <- c("ID", "feature", "lon", "lat", "xy")
# sites_df$sites     <- c("Y")
# sites_df$site      <- c("yardie")
# sites_df$methods   <- c("PREF")
# sites_df$method    <- c("PREF")
# sites_df$pointnum  <- c(1:nrow(sites_df))
# sites_df$dropcode  <- interaction(sites_df$sites, sites_df$methods, 
#                                   sites_df$pointnum, sep = "")
# sites_df <- sites_df[ , colnames(sites_df) %in% 
#                         c("lon", "lat", "dropcode", "site", 
#                           "method", "pointnum", "feature")]
# sites_df$selected <- c("PREF")
# head(sites_df)
# 
# sites_spdf <- SpatialPointsDataFrame(sites_df[2:3], data = sites_df)
# plot(sites_spdf)
# 
# shapefile(sites_spdf, "output/planned/yardie_pref", overwrite = TRUE)
# 



