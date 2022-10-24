###
# Project: Parks OMP Ningaloo
# Data:    BRUVS, BOSS Habitat data
# Task:    Create figure for spatial pie charts
# Author:  Claude Spencer
# Date:    October 2022
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
name <- "Parks-Ningaloo-synthesis"                                              # Change here

# define crs
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Set cropping extent - larger than most zoomed out plot
e <- ext(113, 114.5, -23, -21)

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

dat <- readRDS("data/tidy/Parks-Ningaloo-synthesis_habitat-bathy-derivatives.rds") %>%
  dplyr::rename("Sessile invertebrates" = inverts,
                "Sand" = sand) %>%
  dplyr::mutate(grouping = factor(1:nrow(.))) %>%
  glimpse()

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, e)

#bring in bathy for contour lines
bathy <- readRDS(paste(paste0('data/spatial/rasters/', name), 
                    'spatial_covariates.rds', sep = "_"))
bathy <- rast(bathy)
bathy <- bathy[[1]]
bathdf <- as.data.frame(bathy, xy = T, na.rm = T)
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
hab_fills <- scale_fill_manual(values = c("Sand" = "wheat",
                                          "Sessile invertebrates" = "plum"
                                         ))

# depth colours 
depth_fills <- scale_fill_manual(values = c("#a7cfe0","#9acbec","#98c4f7", 
                                            "#a3bbff", "#81a1fc"), guide = "none")
#shallow to deep

#make the plot

gg.scatterpie <- ggplot() + 
  geom_contour_filled(data = bathdf, aes(x, y, z = Depth, fill = after_stat(level)), color = "black",
                      breaks = c(-30, -70, -200,-700, -2000), size = 0.1) +
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
  geom_scatterpie(aes(x=x, y=y, group= grouping), data=dat,
                  cols = c("Sessile invertebrates", "Sand"),
                  pie_scale = 0.45, color = NA) +
  labs(fill = "Habitat",x = 'Longitude', y = 'Latitude')+
  hab_fills + 
  # annotate("text", x = c(113.47, 113.405, 113.278), y = c(-28.13, -28.13, -28.13), label = c("30m", "70m", "200m"),
  #          size = 1.5, colour = "black")+
  coord_sf(xlim = c(min(dat$x), max(dat$x)),                                    # Set plot limits
           ylim = c(min(dat$y), max(dat$y)))+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "#b9d1d6"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
png(filename = paste(paste0('figures/habitat/', name), 'scatterpies.png', sep = "_"),
    units = "in", height = 8, width = 6, res = 300)
gg.scatterpie
dev.off()


