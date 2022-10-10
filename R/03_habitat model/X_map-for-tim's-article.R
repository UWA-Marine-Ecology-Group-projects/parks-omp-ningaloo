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

# get data and sort spatial boundaries
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif")                     # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
aus    <- aus[aus$FEAT_CODE == c("mainland", "island"), ]
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
st_crs(aus) <- st_crs(aumpa)
wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")                    # all wa mpas
st_crs(wampa) <- st_crs(aumpa)
terrnp <- st_read(
  "data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp")           # terrestrial reserves
cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # coastal waters line

# simplify zone names
nb_nmp$ZoneName <- dplyr::recode(nb_nmp$ZoneName,
                                 "Special Purpose Zone (Mining Exclusion)" =
                                   "Special Purpose Zone\n(Mining Exclusion)")

nb_mp$waname <- gsub("( \\().+(\\))", "", nb_mp$ZONE_TYPE)
nb_mp$waname <- gsub(" [1-4]", "", nb_mp$waname)
# ab_mpa$waname[ab_mpa$ZONE_TYPE == unique(ab_mpa$ZONE_TYPE)[14]] <- 
#   c("Special Purpose Zone\n(Habitat Protection)")
nb_mp$waname[nb_mp$NAME == "Ngari Capes"]     <- "General Use"
nb_mp$waname <- dplyr::recode(nb_mp$waname, 
                              "General Use" = "General Use Zone",
                              # "MMA" = "Marine Management Area",
                              # "Recreation Area" = "Recreation Zone",
                              # "Conservation Area" = "Sanctuary Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)")

# fix up new zones within Ngari Capes
wanew$waname <- word(wanew$Name, start = -2, end = -1)

# reduce terrestrial parks
terrnp <- terrnp[terrnp$leg_catego %in% c("Nature Reserve", "National Park"), ] # exclude state forests etc
terrnp <- st_crop(terrnp, xmin = 110, xmax = 123, ymin = -39, ymax = -33.3)       # just swc
# plot(terrnp["leg_catego"])

# assign commonwealth zone colours
nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1",
                                          "Special Purpose Zone" = "#368ac1"))

nmpa_outs <- scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Habitat Protection Zone" = "#fff8a3",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1",
                                           "Special Purpose Zone" = "#368ac1"))

# state colours
wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952"))

# state terrestrial parks colours
waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"))

# build basic plot elements
p1 <- ggplot() +
  # geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 0.9) +
  # scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour_filled(data = bath_newdf, aes(x = x, y = y, z = Depth,
                                             fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000)) +
  # geom_contour(data = bathdf, aes(x = x, y = y, z = Depth),
  # binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = nb_mp, aes(fill = waname), alpha = 2/5, colour = NA) +
  geom_sf(data = wanew, aes(fill = waname), alpha = 2/5, colour = NA) +
  wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "Terrestrial Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = nb_nmp, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  nmpa_cols + 
  geom_contour(data = bath_newdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), colour = "white",
               alpha = 1, size = 0.1) +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks", title = "b)") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = 114.38, xmax = 115.1, ymin = -34.17, ymax = -33.65,
           colour = "goldenrod1", fill = "white", alpha = 0.2, size = 0.6) +
  coord_sf(xlim = c(111, 122.1), ylim = c(-38.5, -33.3)) +
  # coord_sf(xlim = c(114.3, 115.8), ylim = c(-34.5, -33.3)) +
  theme_minimal()#+
# theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(), 
#       legend.box.just = "left")
# p1

# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = rg_nmp, alpha = 5/6, colour = "grey85", size = 0.02) +
  labs(title = "a)")+
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-40, -13)) +
  annotate("rect", xmin = 110, xmax = 122.1, ymin = -39, ymax = -33.3,
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
ggsave("plots/spatial/overview_map.png", dpi = 200, width = 10, height = 4.5) #6