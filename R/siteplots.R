
###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    Overview maps
# author:  Kingsley Griffin
# date:    Jun/July 2021
##

library(sf)
library(rgeos)
library(ggplot2)
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
cwatr  <- readRDS('output/coastal_waters_limit_trimmed.rds')                    # coastal waters line trimmined in 'R/GA_coast_trim.R'
bathdf <- readRDS("output/ga_bathy_trim.rds")                                   # bathymetry trimmed in 'R/GA_coast_trim.R'
colnames(bathdf)[3] <- "Depth"
st_crs(aus)         <- st_crs(aumpa)

# simplify state parks zone names
ni_mpa$waname <- gsub("( \\().+(\\))", "", ni_mpa$ZONE_TYPE)
ni_mpa$waname <- gsub(" [1-4]", "", ni_mpa$waname)
ni_mpa$waname[ni_mpa$ZONE_TYPE == unique(ni_mpa$ZONE_TYPE)[2]] <- 
  c("Special Purpose Zone (Shore Based Activities)")
ni_mpa$waname <- dplyr::recode(ni_mpa$waname, 
                               "General Use" = "General Use Zone",
                               "Unassigned" = "Unassigned/Unclassified",
                               "MMA" = "Marine Management Area",
                               "Recreation Area" = "Recreation Zone",
                               # "Conservation Area" = "Sanctuary Zone",
                               "Special Purpose Zone (Shore Based Activities)" = 
                                 "Special Purpose Zone\n(Shore Based Activities)")


# assign mpa colours
nmpa_cols <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fff8a3",# Commonwealth MPA colours
                                         # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                         "National Park Zone" = "#7bbc63",
                                         "Multiple Use Zone" = "#b9e6fb",
                                         "Recreational Use Zone" = "#ffb36b",
                                         "Sanctuary Zone" = "#f7c0d8",
                                         # "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                         # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                         "Special Purpose Zone" = "#6daff4"
))

wampa_cols <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fffbcc",# State MPA colours
                                          # "Fish Habitat Protection Area" = "#fbff85",
                                          # "National Park Zone" = "#a4d194",
                                          "General Use Zone" = "#bddde1",
                                          "Recreation Zone" = "#f4e952",
                                          "Sanctuary Zone" = "#bfd054",
                                          "Conservation Area" = "#b3a63d",
                                          "Special Purpose Zone" = "#c5bcc9",
                                          "Marine Management Area" = "#b7cfe1",
                                          "Unassigned/Unclassified" = "#ddccff",
                                          "Special Purpose Zone\n(Shore Based Activities)" = "#ba3030"
))

# build main plot elements
p1 <- ggplot() +
  geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 0.9) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = bathdf, aes(x = x, y = y, z = Depth), 
               binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ni_mpa, aes(fill = waname), alpha = 3/5, colour = NA) +
  wampa_cols +
  labs(fill = "State") +
  new_scale_fill() +
  geom_sf(data = ni_nmp, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  nmpa_cols +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.1) +
  coord_sf(xlim = c(111, 116), ylim = c(-25, -20)) +
  labs(x = NULL, y = NULL, fill = "Commonwealth") +
  guides(fill = guide_legend(order = 1)) +
  theme_minimal()
p1

# add mpa labels
p1 <- p1 +   
  annotate("rect", xmin = 113.2, xmax = 113.9, ymin = -22.9, ymax = -22,        # site box
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("rect", xmin = 111.9, xmax = 112.9, ymin = -21.66, ymax = -21.35,    # mpa label box
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 112.4, y = -21.5, size = 2, colour = "grey20",           # mpa label text
           label = "Gascoyne Marine Park \n (Commonwealth)") +
  annotate("rect", xmin = 113.62, xmax = 114.55, ymin = -23.6, ymax = -23.3,
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 114.1, y = -23.45, size = 2, colour = "grey20",
           label = "Ningaloo Marine Park \n (Commonwealth)") +
  annotate("rect", xmin = 111.75, xmax = 112.8, ymin = -24.65, ymax = -24.35,
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 112.3, y = -24.5, size = 2, colour = "grey20",
           label = "Shark Bay Marine Park \n (Commonwealth)") +
  annotate("rect", xmin = 114.3, xmax = 115.3, ymin = -20.35, ymax = -20.05,
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 114.8, y = -20.2, size = 2, colour = "grey20",
           label = "Montebello Marine Park \n (Commonwealth)")
p1
  
# build inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = nw_mpa, alpha = 5/6, colour = "grey85", size = 0.02) +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 111, xmax = 116, ymin = -25, ymax = -20, 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
p2

# plot both inset and main pane
p2 + p1 + plot_layout(widths = c(0.8, 2.2))

ggsave("figures/locplot.png", dpi = 200, width = 10, height = 8)


## site layout plot

# get sampling data (collated from google sheets in 'R/getdata.R')
tsboss <- readRDS('data/2105_ningaloo_tsboss.rds')
pcbruv <- readRDS('data/2105_ptcloates_bruv.rds')
pcboss <- readRDS('data/2105_ptcloates_boss.rds')

# shorten mpa colour palletes to reduce legend items
snmpa_cols <- scale_fill_manual(values = c(#"Habitat Protection Zone" = "#fff8a3",
                                          # "Habitat Protection Zone (Reefs)" = "#fbff85",
                                          "National Park Zone" = "#7bbc63",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          # "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                          # "Special Purpose Zone (Trawl)" = "#3e8ec4",
                                          # "Special Purpose Zone" = "#6daff4",
                                          "Sanctuary Zone" = "#f7c0d8",
                                          "Recreational Use Zone" = "#ffb36b"
))

swampa_cols <- scale_fill_manual(values = c(#"Habitat Protection Zone" = "#fffbcc",# State MPA colours
                                            # "Fish Habitat Protection Area" = "#fbff85",
                                            # "National Park Zone" = "#a4d194",
                                            "General Use Zone" = "#bddde1",
                                            "Recreation Zone" = "#f4e952",
                                            "Sanctuary Zone" = "#bfd054",
                                            # "Conservation Area" = "#b3a63d",
                                            "Special Purpose Zone" = "#c5bcc9",
                                            # "Marine Management Area" = "#b7cfe1",
                                            "Unassigned/Unclassified" = "#ddccff",
                                            "Special Purpose Zone\n(Shore Based Activities)" = "#ba3030"
                                            ))

# use finer bathy for detailed site map
sitebathy <- readRDS('output/ga_bathy_fine.rds')
colnames(sitebathy)[3] <- "Depth"

p3 <- ggplot() +
  geom_raster(data = sitebathy, aes(x, y, fill = Depth), alpha = 4/5) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = sitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 250, colour = "white", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ni_mpa, aes(fill = waname), 
          alpha = 3/5, colour = NA) +
  swampa_cols +
  labs(fill = "State") +
  new_scale_fill() +
  geom_sf(data = ni_nmp, aes(fill = ZoneName), 
          alpha = 3/5, colour = NA) +
  snmpa_cols +
  geom_sf(data = aus, fill = "seashell2", colour = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.15) +
  annotate("rect", xmin = 113.4, xmax = 113.7, ymin = -22.85, ymax = -22.6, 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("text", x = 113.86, y = -22.38, size = 3, 
           colour = "grey20", label = "Fig. 3.1") +
  annotate("rect", xmin = 113.65, xmax = 113.9, ymin = -22.4, ymax = -22, 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("text", x = 113.44, y = -22.62, size = 3, 
           colour = "grey20", label = "Fig. 3.2") +
  geom_point(data = pcbruv, shape = 10, alpha = 4/5,
             aes(Longitude, Latitude, colour = "BRUV")) +
  geom_point(data = pcboss, shape = 10, alpha = 4/5,
             aes(Longitude, Latitude, colour = "Drop Camera")) +
  geom_point(data = tsboss, shape = 10, alpha = 4/5,
             aes(Longitude, Latitude, colour = "Drop Camera")) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "seagreen4")) +
  coord_sf(xlim = c(113.3, 113.9), ylim = c(-22.85, -22)) +
  labs(colour = "Sample", x = NULL, y = NULL, fill = "Commonwealth") +
  guides(fill = guide_legend(order = 1)) +
  theme_minimal()
p3

ggsave("figures/siteplot.png", dpi = 200, width = 9, height = 12)

# closer to drop areas to show spread relative to variables

# even finer bathy for detailed site map
fsitebathy <- sitebathy[sitebathy$Depth > -1200, ]
sswampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                             "Unassigned/Unclassified" = "#ddccff",
                                             "Special Purpose Zone\n(Shore Based Activities)" = "#ba3030"))

p4 <- ggplot() +
  geom_raster(data = fsitebathy, aes(x, y, fill = Depth), alpha = 0.9) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = fsitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 50, colour = "white", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ni_mpa, aes(fill = waname), 
          alpha = 3/5, colour = NA) +
  sswampa_cols +
  labs(fill = "State") +
  new_scale_fill() +
  geom_sf(data = ni_nmp, aes(fill = ZoneName), 
          alpha = 3/5, colour = NA) +
  snmpa_cols +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.15) +
  geom_point(data = pcbruv, shape = 10, alpha = 0.9,
             aes(Longitude, Latitude, colour = "BRUV")) +
  geom_point(data = pcboss, shape = 10, alpha = 0.9,
             aes(Longitude, Latitude, colour = "Drop Camera")) +
  geom_point(data = tsboss, shape = 10, alpha = 0.9,
             aes(Longitude, Latitude, colour = "Drop Camera")) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "seagreen4")) +
  coord_sf(xlim = c(113.4, 113.7), ylim = c(-22.85, -22.6)) +
  labs(colour = "Sample", x = NULL, y = NULL, fill = "Commonwealth") +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = 113.4, xmax = 113.46, ymin = -22.63, ymax = -22.61,    # mpa label box
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 113.43, y = -22.62, size = 2, colour = "grey20",           # mpa label text
           label = "Gascoyne Marine Park \n (Commonwealth)") +
  annotate("rect", xmin = 113.505, xmax = 113.565, ymin = -22.63, ymax = -22.61,
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 113.535, y = -22.62, size = 2, colour = "grey20",
           label = "Ningaloo Marine Park \n (Commonwealth)") +
  annotate("rect", xmin = 113.6, xmax = 113.66, ymin = -22.63, ymax = -22.61,
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 113.63, y = -22.62, size = 2, colour = "grey20",
           label = "Ningaloo Marine Park \n (State)") +
  theme_minimal()
p4
ggsave("figures/sthsite.png", dpi = 200, width = 10, height = 8)

snmpa_cols <- scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                                           "Sanctuary Zone" = "#f7c0d8",
                                           "Recreational Use Zone" = "#ffb36b"))
fsitebathy <- sitebathy[sitebathy$Depth > -1400, ]
p5 <- ggplot() +
  geom_raster(data = fsitebathy, aes(x, y, fill = Depth), alpha = 0.9) +
  scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour(data = fsitebathy, aes(x = x, y = y, z = Depth), 
               binwidth = 50, colour = "white", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = ni_mpa, aes(fill = waname), 
          alpha = 2/5, colour = "grey90", size = 0.1) +
  swampa_cols +
  labs(fill = "State") +
  new_scale_fill() +
  geom_sf(data = ni_nmp, aes(fill = ZoneName), 
          alpha = 2/5, colour = "grey90", size = 0.15) +
  snmpa_cols +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.05) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.15) +
  geom_point(data = pcboss, shape = 10, alpha = 4/5,
             aes(Longitude, Latitude, colour = "Drop Camera")) +
  geom_point(data = tsboss, shape = 10, alpha = 4/5,
             aes(Longitude, Latitude, colour = "Drop Camera")) +
  scale_colour_manual(values = c("Drop Camera" = "springgreen4")) +
  coord_sf(xlim = c(113.65, 113.9), ylim = c(-22.36, -22.03)) +
  labs(colour = "Sample", x = NULL, y = NULL, fill = "Commonwealth") +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = 113.645, xmax = 113.715, ymin = -22.04, ymax = -22.02,    # mpa label box
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 113.68, y = -22.03, size = 2, colour = "grey20",           # mpa label text
           label = "Gascoyne Marine Park \n (Commonwealth)") +
  annotate("rect", xmin = 113.765, xmax = 113.835, ymin = -22.04, ymax = -22.02,
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 113.8, y = -22.03, size = 2, colour = "grey20",
           label = "Ningaloo Marine Park \n (Commonwealth)") +
  annotate("rect", xmin = 113.845, xmax = 113.91, ymin = -22.04, ymax = -22.02,
           colour = "grey60", fill = "white", alpha = 2/5, size = 0.2) +
  annotate("text", x = 113.88, y = -22.03, size = 2, colour = "grey20",
           label = "Ningaloo Marine Park \n (State)") +
  theme_minimal()
p5
ggsave("figures/nthsite.png", dpi = 200, width = 8, height = 10)

