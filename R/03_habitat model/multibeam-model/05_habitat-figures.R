###
# Project: Parks OMP Ningaloo
# Data:    BRUVS, BOSS Habitat data
# Task:    Plot prediction figures
# Author:  Claude Spencer & Stanley Mastrantonis
# Date:    October 2022
##


# Clear your environment
rm(list = ls())

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)
library(scales)

# Set your study name
name <- "Parks-Ningaloo-synthesis"                                              # Change here

dat <- readRDS(paste(paste0('data/tidy/', name), 
                     'nesp-habitat-bathy-derivatives.rds', sep = "_")) %>%
  dplyr::mutate(dom_tag = ifelse((inverts/broad.total.points.annotated) > 0.5, "inverts", 
                                 ifelse((inverts/broad.total.points.annotated) < 0.5 & 
                                          (inverts/broad.total.points.annotated) > 0.1, "sparse inverts", "sand"))) %>%
  glimpse()

stack <- readRDS(paste(paste0('data/spatial/rasters/raw bathymetry/', name),      # This is ignored - too big!
                       'spatial_covariates.rds', sep = "_")) %>%
  rast()

# pred_class <- rast(paste0("output/rf-habitat/", name, "_nesp_predicted-habitat.tif"))  %>%
#   focal(w = c(9,9), fun = "modal") %>%
#   focal(w = c(9,9), fun = "modal") # Filtered multiple times to get rid of artefacts

pred_classdf <- as.data.frame(pred_class, xy = T, na.rm = T) %>%
  dplyr::rename(layer_value = "category") %>%                                   # SOmetimes changes? package conflict?
  dplyr::mutate(layer_value = recode(layer_value,
                                     "sand" = "Sand",
                                     "inverts" = "Sessile invertebrates",
                                     "sparse inverts" = "Sparse invertebrates")) %>%
  glimpse()

# pred_classdf <- as.data.frame(pred_class, xy = T, na.rm = T) %>%
#   dplyr::rename(layer_value = "focal_modal") %>%                                   # SOmetimes changes? package conflict?
#   dplyr::mutate(layer_value = recode(layer_value,
#                                      "2" = "Sand",
#                                      "1" = "Sessile invertebrates")) %>%
#   glimpse()

pred_classsp <- vect(pred_classdf, geom = c("x", "y"),
                                       crs = "EPSG:32749")
predclasssp <- project(pred_classsp, "epsg:4326")
pred_classdf <- as.data.frame(predclasssp, geom = "XY")
# pred_classdf <- cbind(pred_classdf, terra::extract(stack[[1]], pred_classsp)) %>%
#   # dplyr::filter(Z > -220 & Z < -50) %>%
#   glimpse()

# Set CRS for shapefiles
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"          # Replace with EPSG:4283
sppcrs <- "EPSG:32749"

# Assign habitat class colours
hab_fills <- scale_fill_manual(values = c(
  "Sand" = "wheat",
  "Sparse invertebrates" = "#d48aa8",
  "Sessile invertebrates" = "#673147"
))

# Set cropping extent - larger than most zoomed out plot
e <- ext(113, 114.5, -23, -21)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
st_crs(aus) <- gdacrs
ausc <- st_crop(aus, e)
ausc <- st_transform(ausc, sppcrs)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
mpa <- st_crop(aumpa, e)                                                        # Crop to the study area
mpa <- st_transform(mpa, sppcrs)
# Reorder levels so everything plots nicely
mpa$ZoneName <- factor(mpa$ZoneName, levels = c("Recreational Use Zone",
                                                "Multiple Use Zone", 
                                                "Special Purpose Zone",
                                                "National Park Zone"))
npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Just National Park Zones

# State parks
wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")                    # All aus mpas
st_crs(wampa) <- st_crs(aumpa)
wampa <- st_crop(wampa, e)                                                      # Crop to the study area
wampa <- st_transform(wampa, sppcrs)
# simplify zone names
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Ngari Capes"]     <- "General Use"
wampa$waname <- dplyr::recode(wampa$waname, 
                              "General Use" = "General Use Zone",
                              # "MMA" = "Marine Management Area",
                              # "Recreation Area" = "Recreation Zone",
                              # "Conservation Area" = "Sanctuary Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)")

unique(wampa$waname)
sanc <- wampa %>%
  dplyr::filter(waname %in% "Sanctuary Zone")

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, e)  
cwatr <- st_transform(cwatr, sppcrs)

# Bathymetry
bathdf <- as.data.frame(stack[[1]], na.rm = T, xy = T)

#Build plot elements for site 1
p1 <- ggplot() +
  # geom_tile(data = bathdf, aes(x, y, fill = Z), show.legend = F) +
  # scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
  #                      values = rescale(c(-2437, -120, 0))) +
  # new_scale_fill() +
  geom_tile(data = pred_classdf, aes(x, y, fill = layer_value)) +
  hab_fills + 
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = npz, fill = NA, colour = "#7bbc63") +                          # Add national park zones
  geom_sf(data = sanc, fill = NA, colour = "#bfd054") +                         # Add national park zones
  geom_sf(data = cwatr, fill = NA, colour = "red", size = 0.3) +
  # geom_contour(data = bathdf, aes(x = x, y = y, z = Z),                         # Contour lines
  #              breaks = c(- 30, -70, - 200),                                    # Contour breaks - change to binwidth for regular contours
  #              colour = "grey54",
  #              alpha = 1, size = 0.5) +                                       # Transparency and linewidth
  geom_point(data = dat, aes(x = x, y = y, fill = dom_tag), colour = "black",
             pch = 21, alpha = 1, show.legend = F) +
  scale_fill_manual(values = c(  "sand" = "wheat",
                                 "sparse inverts" = "#d48aa8",
                                 "inverts" = "#673147")) +
  coord_sf(xlim = c(min(pred_classdf$x), max(pred_classdf$x)),
           ylim = c(min(pred_classdf$y), max(pred_classdf$y))) +
  labs(x = NULL, y = NULL, fill = "Habitat",                                    # Labels  
       colour = NULL) +
  # annotate("text", x = c(113.428836237, 113.388204915, 113.255153069),          # Add contour labels manually
  #          y = c(-28.078038504, -28.078038504, -28.078038504), 
  #          label = c("30m", "70m", "200m"),
  #          size = 2, colour = "grey54") +
  theme_minimal()
# png(filename = paste(paste("figures/habitat", name, sep = "/"),                 # Save output
#                      "nesp-dominant_habitat.png", sep = "_"),
#     width = 6, height = 8, res = 300, units = "in")                             # Change the dimensions here as necessary
p1
dev.off()

p2 <- ggplot() +
  geom_tile(data = pred_classdf, aes(x, y, fill = layer_value)) +
  hab_fills + 
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = npz, fill = NA, colour = "#7bbc63") +                          # Add national park zones
  geom_sf(data = sanc, fill = NA, colour = "#bfd054") +                         # Add national park zones
  geom_sf(data = cwatr, fill = NA, colour = "red", size = 0.3) +                
  geom_point(data = dat, aes(x = x, y = y, fill = dom_tag), colour = "black",
             pch = 21, alpha = 1, show.legend = F) +
  scale_fill_manual(values = c(  "sand" = "wheat",
                                 "sparse inverts" = "#d48aa8",
                                 "inverts" = "#673147")) +
  coord_sf(xlim = c(750000, 775000),
           ylim = c(7500000, 7470000)) +
  labs(x = NULL, y = NULL, fill = "Habitat",                                    # Labels  
       colour = NULL) +
  theme_minimal()
# png(filename = paste(paste("figures/habitat", name, sep = "/"),                 # Save output
#                      "nesp-dominant_habitat.png", sep = "_"),
#     width = 6, height = 8, res = 300, units = "in")                             # Change the dimensions here as necessary
p2
# dev.off()

unique(pred_classdf$layer_value)
# Put some spatial uncertainty type plot here!



################# TEST GAMS ##################
preddf <- as.data.frame(stack, xy = T, na.rm = T)

library(mgcv)
m_sand <- gam(cbind(sand, broad.total.points.annotated - sand) ~ 
                 s(Z,     k = 5, bs = "cr")  + 
                 s(roughness, k = 5, bs = "cr") + 
                 s(detrended, k = 5, bs = "cr"), 
               data = dat, method = "REML", family = binomial("logit"))
summary(m_sand)

m_inverts <- gam(inverts/broad.total.points.annotated ~ 
                s(Z,     k = 5, bs = "cr")  + 
                s(roughness, k = 5, bs = "cr") + 
                s(detrended, k = 5, bs = "cr"), 
              data = dat, method = "REML", family = binomial("logit"))
summary(m_inverts)

preddf <- cbind(preddf,
                "psand" = predict(m_sand, preddf, type = "response"),
                "pinverts" = predict(m_inverts, preddf, type = "response"))

# preddf$dom_tag <- apply(preddf[11:12], 1,
#                          FUN = function(x){names(which.max(x))})

preddf <- preddf %>%
  dplyr::mutate(dom_tag = ifelse(pinverts > 0.5, "inverts", 
                               ifelse((inverts/broad.total.points.annotated) < 0.5 & 
                                        (inverts/broad.total.points.annotated) > 0.1, "sparse inverts", "sand")))

summary(preddf)

p3 <- ggplot() +
  geom_tile(data = preddf, aes(x, y, fill = dom_tag)) +
  scale_fill_manual(values = c("pinverts" = "plum",
                               "psand" = "wheat")) + 
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = npz, fill = NA, colour = "#7bbc63") +                          # Add national park zones
  geom_sf(data = sanc, fill = NA, colour = "#bfd054") +                         # Add national park zones
  geom_sf(data = cwatr, fill = NA, colour = "red", size = 0.3) +                
  # geom_point(data = dat, aes(x = x, y = y, fill = dom_tag), colour = "black",
  #            pch = 21, alpha = 1, show.legend = F) +
  # scale_fill_manual(values = c(  "sand" = "wheat",
  #                                "sparse inverts" = "#d48aa8",
  #                                "inverts" = "#673147")) +
  coord_sf(xlim = c(min(preddf$x), max(preddf$x)),
           ylim = c(min(preddf$y), max(preddf$y))) +
  labs(x = NULL, y = NULL, fill = "Habitat",                                    # Labels  
       colour = NULL) +
  theme_minimal()
# png(filename = paste(paste("figures/habitat", name, sep = "/"),                 # Save output
#                      "nesp-dominant_habitat.png", sep = "_"),
#     width = 6, height = 8, res = 300, units = "in")                             # Change the dimensions here as necessary
p3

