
###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS
# Task:    Overview maps
# author:  Kingsley Griffin
# date:    Jun 2021
##

library(sf)
library(rgeos)
library(ggplot2)
library(viridis)
library(patchwork)
library(raster)
library(ggnewscale)

# get and sort spatial boundaries
aus    <- st_read("data/spatial/shp/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aus    <- aus[aus$FEAT_CODE == "mainland", ]
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
wampa  <- st_read("data/spatial/shp/WA_MPA_2018.shp")                           # all wa mpas
ni_mpa <- wampa[wampa$NAME %in% c("Ningaloo", "Hamelin Pool", "Shark Bay",
                                  "Montebello Islands", "Great Sandy Island",
                                  "Miyaboolya Beach", "Barrow Island",
                                  "Thevenard Island", "Muiron Islands"), ]      # just wa parks nearby
nw_mpa <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # just W nat parks
ni_nmp <- nw_mpa[nw_mpa$ResName %in% c("Abrolhos", "Shark Bay", "Ningaloo", 
                                       "Gascoyne", "Montebello", "Dampier"), ]  # just nat parks nearby
bathdf <- readRDS("output/ga_bathy_trim.rds")                                   # bathymetry trimmed in 'R/GA_coast_trim.R'
colnames(bathdf)[3] <- "Depth"
st_crs(aus)         <- st_crs(aumpa)

# simplify state parks zone names
ni_mpa$waname <- gsub("( \\().+(\\))", "", ni_mpa$ZONE_TYPE)
ni_mpa$waname <- gsub(" [1-4]", "", ni_mpa$waname)
ni_mpa$waname <- dplyr::recode(ni_mpa$waname, 
                               "General Use" = "General Use Zone",
                               "Unassigned" = "Unassigned/Unclassified",
                               "MMA" = "Unassigned/Unclassified",
                               "Recreation Area" = "Recreation Zone",
                               "Conservation Area" = "Sanctuary Zone")

# assign mpa colours
nmpa_cols <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                         # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                         "National Park Zone" = "#7bbc63",
                                         "Multiple Use Zone" = "#b9e6fb",
                                         "Recreational Use Zone" = "#ffb36b",
                                         "Sanctuary Zone" = "#f7c0d8",
                                         # "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                         # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                         "Special Purpose Zone" = "#6daff4"
))

wampa_cols <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fffbcc",
                                          # "Fish Habitat Protection Area" = "#fbff85",
                                          "National Park Zone" = "#a4d194",
                                          "General Use Zone" = "#e7f6fe",
                                          "Recreation Zone" = "#ffd7b3",
                                          "Sanctuary Zone" = "#fce8f1",
                                          # "Conservation Area" = "#fce8f1",
                                          "Special Purpose Zone" = "#b8d8fa",
                                          "Unassigned/Unclassified" = "#ddccff"
))

# build basic plot elements
p1 <- ggplot() +
  geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Depth), 
               binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  geom_sf(data = aus, fill = "grey85", colour = "grey80") +
  new_scale_fill() +
  geom_sf(data = ni_mpa, aes(fill = waname),
          alpha = 3/5, colour = "grey90", size = 0.1) +
  wampa_cols +
  labs(fill = "State") +
  new_scale_fill() +
  geom_sf(data = ni_nmp, aes(fill = ZoneName),
          alpha = 4/5, colour = "grey90", size = 0.15) +
  nmpa_cols +
  annotate("rect", xmin = 113.2, xmax = 113.9, ymin = -22.9, ymax = -22,
           colour = "grey25", alpha = 1/5, size = 0.2) +
  coord_sf(xlim = c(111, 116), ylim = c(-25, -20)) +
  labs(x = NULL, y = NULL, fill = "Commonwealth") +
  guides(fill = guide_legend(order = 1)) +
  theme_minimal()
p1

# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "grey95", colour = "grey90", size = 0.01) +
  geom_sf(data = nw_mpa, alpha = 5/6, colour = "grey85", size = 0.02) +
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 110, xmax = 116, ymin = -25, ymax = -20, 
           colour = "grey25", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
p2

# plot both 
p2 + p1 + plot_layout(widths = c(0.8, 2.2))

ggsave("figures/locplot.png", dpi = 200)


## site zoom plot

# get sampling data (collated from google sheets in 'R/getdata.R')
tsboss <- readRDS('data/2105_ningaloo_tsboss.rds')
pcbruv <- readRDS('data/2105_ptcloates_bruv.rds')
pcboss <- readRDS('data/2105_ptcloates_boss.rds')

# assign mpa colours
nmpa_cols <- scale_fill_manual(values = c(#"Habitat Protection Zone" = "#fff8a3",
                                          # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                          "National Park Zone" = "#7bbc63",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Recreational Use Zone" = "#ffb36b",
                                          # "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                          # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                          # "Special Purpose Zone" = "#6daff4",
                                          "Sanctuary Zone" = "#f7c0d8"
))

wampa_cols <- scale_fill_manual(values = c(#"Habitat Protection Zone" = "#fffbcc",
                                           # "Fish Habitat Protection Area" = "#fbff85",
                                           # "National Park Zone" = "#a4d194",
                                           "General Use Zone" = "#e7f6fe",
                                           "Recreation Zone" = "#ffd7b3",
                                           "Sanctuary Zone" = "#fce8f1",
                                           # "Conservation Area" = "#fce8f1",
                                           "Special Purpose Zone" = "#b8d8fa",
                                           "Unassigned/Unclassified" = "#ddccff"
))

# fine bathy for detailed site map
sitebathy <- readRDS('output/ga_bathy_fine.rds')
colnames(sitebathy)[3] <- "Depth"

p3 <- ggplot() +
  geom_raster(data = sitebathy, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = sitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 250, colour = "white", alpha = 4/5, size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ni_mpa, aes(fill = waname), 
          alpha = 2/5, colour = "grey90", size = 0.1) +
  wampa_cols +
  labs(fill = "State") +
  new_scale_fill() +
  geom_sf(data = ni_nmp, aes(fill = ZoneName), 
          alpha = 3/5, colour = "grey90", size = 0.15) +
  nmpa_cols +
  geom_sf(data = aus, fill = "grey90", colour = "grey80") +
  geom_point(data = pcbruv, shape = 3, alpha = 4/5,
             aes(Longitude, Latitude, colour = "BRUV")) +
  geom_point(data = pcboss, shape = 3, alpha = 4/5,
             aes(Longitude, Latitude, colour = "Drop Camera")) +
  geom_point(data = tsboss, shape = 3, alpha = 4/5,
             aes(Longitude, Latitude, colour = "Drop Camera")) +
  scale_colour_manual(values = c("BRUV" = "grey20",
                               "Drop Camera" = "grey60")) +
  coord_sf(xlim = c(113.3, 113.9), ylim = c(-22.85, -22)) +
  labs(colour = "Sample", x = NULL, y = NULL, fill = "Commonwealth") +
  guides(fill = guide_legend(order = 1)) +
  theme_minimal()
p3

ggsave("figures/siteplot.png", dpi = 200)

