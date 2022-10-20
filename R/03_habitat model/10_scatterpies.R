###
# Project: ** Add here **
# Data:    BOSS & BRUV Habitat data
# Task:    Make scatterpie habitat visualisation
# author:  Claude Spencer
# date:    ** Add here **
##

rm(list=ls())

library(dplyr)
library(ggplot2)
library(scatterpie)
library(viridis)
library(sf)
library(terra)
library(ggnewscale)
library(metR)
library(cowplot)

# Set your study name
name <- "Abrolhos"                                                              # Change here

#define crs
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Set cropping extent - larger than most zoomed out plot
e <- ext(112, 116, -30, -26)

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
aumpa$ZoneName <- factor(aumpa$ZoneName, levels = c("Multiple Use Zone", 
                                                    "Special Purpose Zone",
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
                                "Special Purpose Zone")

wampa <- st_crop(wampa, e)                                                      # Crop to the study area
wasanc <- wampa[wampa$waname %in% "Sanctuary Zone", ]

dat <- readRDS("data/tidy/Abrolhos_habitat-bathy-derivatives.rds") %>%
  dplyr::rename("Sessile invertebrates" = inverts,
                "Rock" = rock,
                "Macroalgae" = macroalgae,
                "Sand" = sand,
                "Kelp" = kelps) %>%
  dplyr::mutate(grouping = factor(1:nrow(.))) %>%
  glimpse()

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, e)

#bring in bathy for contour lines
bathdf <- readRDS(paste(paste0('data/spatial/rasters/',                         # From 02_spatial_layers.R
                                         name), 'ga_bathy.rds', sep = "_"))                 
colnames(bathdf)[3] <- "Depth"
# assign commonwealth zone colours
nmpa_fills <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1")) 

# state colours
wampa_fills <- scale_fill_manual(values = c("Fish Habitat Protection Area" = "#fac86b",
                                           "Reef Observation Area" = "#ddccff",
                                           "Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952",
                                           "Special Purpose Zone" = "#c5bcc9",
                                           "Marine Nature Reserve" = "#bfd054"))

#class colours 
hab_fills <- scale_fill_manual(values = c("Rock" = "grey40",
                                         "Sessile invertebrates" = "plum",
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Kelp" = "goldenrod1",
                                         "Sand" = "wheat"))

# depth colours 
depth_fills <- scale_fill_manual(values = c("#a7cfe0","#9acbec","#98c4f7", 
                                            "#a3bbff"), guide = "none")
#shallow to deep

#make the plot

gg.scatterpie <- ggplot() + 
  geom_contour_filled(data = bathdf, aes(x, y, z = Depth, fill = after_stat(level)), color = "black",
                      breaks = c(-30, -70, -200,-700,-10000), size = 0.1) +
  annotate("text", x = c(114.40,114.467,114.72,114.945), y = -33.85, label = c("700m","200m","70m","30m"), size = 2)+
  depth_fills +
  new_scale_fill()+
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = wampa,fill = "#bfd054", alpha = 2/5, color = NA)+
  wampa_fills +
  labs(fill = "State Marine Parks")+
  new_scale_fill()+
  geom_sf(data = npz, fill = "#7bbc63",alpha = 2/5, color = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  new_scale_fill() +
  geom_scatterpie(aes(x=longitude, y=latitude, group=grouping), data=dat,
                  cols = c("Rock","Sessile invertebrates","Macroalgae",
                           "Kelp", "Sand"),
                  pie_scale = 0.45, color = NA) +
  labs(fill = "Habitat",x = 'Longitude', y = 'Latitude', title = "Shallow Bank")+
  hab_fills + 
  annotate("text", x = c(113.47, 113.405, 113.278), y = c(-28.13, -28.13, -28.13), label = c("30m", "70m", "200m"),
           size = 1.5, colour = "black")+
  coord_sf(xlim = c(113.169637818, 113.592952023), ylim = c(-28.147530871, -27.951387524))+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#b9d1d6"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
gg.scatterpie

save_plot(paste(paste0('plot/habitat/', name), 'scatterpies.png', sep = "_"), 
          gg.scatterpie, base_height = 6.5, base_width = 7)

