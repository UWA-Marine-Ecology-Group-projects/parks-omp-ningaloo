###
# Project: Parks our Marine Parks Ningaloo
# Data:    250m GA bathymetry, AMP and SMP boundaries
# Task:    Map for Tim's article
# author:  Claude Spencer
# date:    October 2022
##

rm(list=ls())

library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
library(stringr)
library(patchwork)
library(terra)
library(ggnewscale)
library(tidyverse) 
library(scales) # Have to load this after terra for rescale to work


# Data for Nik
# test <- readRDS("output/MBH design/nesp_5m_bathy_interp_ptcloates.rds")
# plot(test)
# writeRaster(test, "data/spatial/rasters/raw bathymetry/ptcloates_5m_UTM_interp.tiff", 
#             overwrite = T)

nicbath <- rast("data/spatial/rasters/raw bathymetry/ptcloates_5m_nik-bathy.tif")
wgscrs <- "+proj=longlat +datum=WGS84"
sppcrs <- "+proj=utm +zone=49 +south +datum=WGS84 +units=m +no_defs"       # crs for sp objects
crs(nicbath) <- sppcrs
nicbath <- project(nicbath, wgscrs)
ext(nicbath)

# get data and sort spatial boundaries
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  # dplyr::filter(!FEAT_CODE %in% "sea") %>%
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
st_crs(aus) <- st_crs(aumpa)
cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # coastal waters line
heri <- st_read("data/spatial/shapefiles/world_heritage_public.shp") %>%
  dplyr::filter(NAME %in% "The Ningaloo Coast")

# Load bathymetry data for plotting
cbaths <- list.files("data/spatial/rasters/raw bathymetry", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 0, ]
bath_r <- rast(cbathy)
bath_r <- crop(bath_r, ext(111, 117, -25, -19))
plot(bath_r)
bathdf <- as.data.frame(bath_r, xy = T, na.rm = T)

# simplify zone names
aumpa$ZoneName <- dplyr::recode(aumpa$ZoneName,
                                 "Special Purpose Zone (Mining Exclusion)" =
                                   "Special Purpose Zone\n(Mining Exclusion)")

wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")                    # all wa mpas
wampa <- st_crop(wampa, ext(110, 116, -26, -20)) # 112, 116), ylim = c(-24.5, -20
st_crs(wampa) <- st_crs(aumpa)
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Ngari Capes"]     <- "General Use"
wampa <- wampa %>%
  dplyr::mutate(waname = ifelse(waname %in% "Unassigned", TYPE, waname)) %>%
  glimpse()
wampa$waname <- dplyr::recode(wampa$waname, 
                              "General Use" = "General Use Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                              "Special Purpose Zone\n(Shore Based Activities)",
                              "Marine Management Area" = "MMA")


# reduce terrestrial parks
terrnp <- st_read(
  "data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp")           # terrestrial reserves
terrnp <- terrnp[terrnp$leg_catego %in% c("Nature Reserve", "National Park"), ] # exclude state forests etc

# assign commonwealth zone colours
nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1",
                                          "Special Purpose Zone" = "#368ac1",
                                          "Recreational Use Zone" = "#f4ca76"))

nmpa_outs <- scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Habitat Protection Zone" = "#fff8a3",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1",
                                           "Special Purpose Zone" = "#368ac1"))

# state colours
wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952",
                                           "Fish Habitat Protection Area" = "#fbff85",
                                           "Special Purpose Zone" = "#c5bcc9",
                                           "Recreation Zone" = "#f4e952",
                                           "Recreation Area" = "#f4e952",
                                           "General Use Zone" = "#bddde1",
                                           "General Use" = "#bddde1",
                                           "Conservation Area" = "#b3a63d",
                                           "Conservation Park" = "#b3a63d",
                                           "Marine Management Area" = "#b7cfe1",
                                           "MMA" = "#b7cfe1",
                                           "5(1)(g) Reserve" = "#bddde1"))

# state terrestrial parks colours
waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"))

# build basic plot elements
p1 <- ggplot() +
  geom_raster(data = bathdf, aes(x = x, y = y, fill = Z),show.legend = F) +
  scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
                       values = rescale(c(-5696, -120, 0))) +
  new_scale_fill() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = wampa, aes(fill = waname), alpha = 1, colour = NA) +
  wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "Terrestrial Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = aumpa, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  nmpa_cols + 
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks", title = "b)") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 5, size = 0.5) +
  geom_sf(data = heri, fill = NA, color = "grey18", alpha = 1) +
  annotate(geom = "rect", xmin = 113.485009644915, xmax = 113.661711520183, 
           ymin = -22.7605962651795, ymax = -22.66265703905  ,
           colour = "goldenrod1", fill = "white", alpha = 0.2, size = 0.6) +
  annotate(geom = "point", x = c(114.1279, 115.1052), y = c(-21.9323, -21.6672)) +
  annotate(geom = "text", x = c((114.1279 + 0.35), (115.1052 + 0.35)), 
           y = c(-21.9323, -21.6672), size = 2.5,
           label = c("Exmouth", "Onslow")) +
  annotate(geom = "text", x = c(114.3, 115.5), y = c(-23, -22),
           label = c("Gascoyne", "Pilbara"), fontface = "italic", size = 3) +
  # annotate(geom = "rect", xmin = 112, xmax = 116, ymin = -24.5, ymax = -20,
  #          colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  # guides(fill = guide_legend(order = 1)) +
  theme(legend.position = "none",
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank()) +
  coord_sf(xlim = c(112, 116), ylim = c(-24.5, -20)) +
  theme_minimal()
p1

# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = aumpa, alpha = 5/6, colour = "grey85", size = 0.02) +
  labs(title = "a)")+
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-40, -13)) +
  annotate("rect", xmin = 112, xmax = 116, ymin = -24.5, ymax = -20,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
# p2

# plot both 
p2 + p1 + plot_layout(widths = c(0.8, 2.2))

# ggsave("plots/overview_map.png", dpi = 200, width = 10, height = 6)
ggsave("figures/spatial/overview_map.png", dpi = 200, width = 10, height = 4.5) #6
