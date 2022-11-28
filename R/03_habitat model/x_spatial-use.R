###
# Project: Parks OMP Ningaloo
# Data:    Spatial use data
# Task:    Plots of spatial use in the Ningaloo Marine Park
# author:  Claude & Nicole Hamre
# date:    November 2022
##

# Set directories----
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(scales)
library(viridis)
library(patchwork)

# Set your study name
name <- "Parks-Ningaloo-synthesis"  

# read data 
dat <- readRDS("data/tidy/Parks-Ningaloo-synthesis_Hamre_ning_dat.rds") %>%
  dplyr::filter(Agreement == "Yes") %>%
  dplyr::filter(!is.na(UseLat), !is.na(UseLong)) %>% 
  glimpse()

# spatial data
spdat <- dat %>% 
  dplyr::filter(!is.na(UseLat), !is.na(UseLong)) %>% 
  st_as_sf(coords = c("UseLong", "UseLat"), crs = 4283)

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Set cropping extent - larger than most zoomed out plot
e <- ext(112, 115, -24, -21)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
st_crs(aus) <- gdacrs
ausc <- st_crop(aus, e)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
mpa <- st_crop(aumpa, e)                                                        # Crop to the study area
# Reorder levels so everything plots nicely
unique(mpa$ZoneName)
mpa$ZoneName <- factor(mpa$ZoneName, levels = c("Multiple Use Zone", 
                                                "Recreational Use Zone",
                                                "Habitat Protection Zone",
                                                "National Park Zone"))
npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Just National Park Zones

# State parks
wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")
st_crs(wampa) <- gdacrs
# Simplify names for plot legend
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
wampa$waname[wampa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"
wampa$waname <- dplyr::recode(wampa$waname, 
                              "General Use" = "General Use Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)",
                              "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = 
                                "Special Purpose Zone",
                              "MMA" = 'Marine Management Area' )

wampa <- st_crop(wampa, e)                                                      # Crop to the study area
wasanc <- wampa[wampa$ZONE_TYPE %in% "Sanctuary Zone (IUCN IA)", ]

# Terrestrial parks
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
terrnp <- st_crop(terrnp, e)       # Crop to the study area - using a different extent as this is on land

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, e)

# Bathymetry data
bath_r <- rast("data/spatial/rasters/raw bathymetry/bath_250_good.tif")
bath_r <- crop(bath_r, e)
bath_df <- as.data.frame(bath_r, xy = T, na.rm = T)                             # Dataframe - cropped and above 0 use for bath cross section
bath_r <- clamp(bath_r, upper = 0, value = F)                                   # Only data below 0
bathy <- as.data.frame(bath_r, xy = T, na.rm = T) %>%
  dplyr::rename(Z = bath_250_good)


terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                guide = "none")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Recreational Use Zone" = "#ffb36b"), guide = "none")

# Kernel density plot

p1 <- ggplot() +
  geom_tile(data = bathy, aes(x, y, fill = Z), show.legend = F, alpha = 0.9) +
  scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
                       values = rescale(c(-5066, -120, 0))) +
  new_scale_fill() +
  stat_density_2d(data = dat, aes(x = UseLong, y = UseLat, fill = stat(level), alpha = ..level..), geom = "polygon", bins = 10) +
  scale_fill_viridis(option = "B", name = "Kernel\nDensity") +
  scale_alpha(guide = 'none') +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  geom_sf(data = wasanc, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.6) +
  new_scale_color() +
  coord_sf(xlim = c(113.4, 114.35), ylim = c(-23.6, -21.5)) +
  labs(x = NULL, y = NULL, title = "a)") +
  theme(axis.text.x = element_text(size = 8))
p1

p2 <- ggplot() +
  geom_tile(data = bathy, aes(x, y, fill = Z), show.legend = F, alpha = 0.9) +
  scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
                       values = rescale(c(-5066, -120, 0))) +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_point(data = dat, aes(x = UseLong, y = UseLat, colour = ActivityType), size = 1) +
  scale_colour_manual(values = c("Both" = "lightsalmon1",
                                 "Extractive" = "goldenrod3",
                                 "Non-Extractive" = "gold")) +
  labs(colour = "Activity Type") +
  new_scale_colour() +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  new_scale_color() +
  geom_sf(data = wasanc, fill = NA, colour = "#bfd054") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.6) +
  coord_sf(xlim = c(113.4, 114.35), ylim = c(-23.6, -21.5)) +
  labs(x = NULL, y = NULL, title = "b)") +
  theme(legend.key = element_rect(colour = NA, fill = NA)) +
  theme(axis.text.x = element_text(size = 8))
p2

png(filename = paste0("figures/spatial/", name, "_spatial-use_kernel.png"), 
    units = "in", res = 300, height = 6, width = 9)
p1 + p2
dev.off()
