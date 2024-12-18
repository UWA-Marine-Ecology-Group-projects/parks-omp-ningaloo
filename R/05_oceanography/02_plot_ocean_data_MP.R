###
# Project: Parks OMP Ningaloo
# Data:    Oceanography - SST, SLA, currents & acidification
# Task:    Plot oceanography trends
# author:  Jess Kolbusz & Claude
# date:    November 2022
##

# Clear memory----
rm(list=ls())
gc()

#remotes::install_github("hvillalo/satin2") #for quiver plots in R
library(sf)
library(reshape2)
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)

Zone = "Ningaloo"

locations <-   read.csv("data/spatial/oceanography/network_scale_boundaries.csv", header = TRUE) %>%
  glimpse()

#i use the "zone" column for each since it distinguishes them all spatially
Zone <- 'Ningaloo' #NW or SW
locs <- locations[locations$Zone %in% c(Zone), ]               # just wa parks nearby

# 113.266 114.511 -24.102 -21.62
#lims of the spatial plots # change for each mp, bigger than you think because of arrrows #
xxlim = c(113.266, 114.511) #all NW c(114, 117)#ABR c(112.8, 115) #long
yylim = c(-24.102, -21.62) #all NW c(-21.5, -19) #ABR  #lat

#all this is on git already - i don't know how to code in different for git??
# only one I use for the plots so far is the "aus" one for the coast outline
#setting up mapping/coastal are for spatial, taken from kingsley script X_siteplots
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") #data/spatial/shp/cstauscd_r.mif")                            # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
dirkh  <- aus[aus$ISLAND_NAME == "DIRK HARTOG ISLAND", ]                        # just dirk hartog island
aus    <- aus[aus$FEAT_CODE == "mainland", ]
#extra bits haven't used or loaded for these maps
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")         # all aus mpas
wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2018.shp")                           # all wa mpas
ab_mpa <- wampa[wampa$NAME %in% c("Montebello Islands", #"Jurien Bay", "Ningaloo",
                                  "Hamelin Pool", "Shark Bay"), ]               # just wa parks nearby
NW_mpa <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # just W nat parks
ab_nmp <- NW_mpa[NW_mpa$ResName %in% c("Montebello", "Jurien", "Shark Bay"), ]    # just nat parks nearby
cwatr  <- st_read('data/spatial/shapefiles/amb_coastal_waters_limit.shp')                    # coastal waters line trimmed in 'R/GA_coast_trim.R'
# bathdf <- readRDS("output/ga_bathy_trim.rds")                                   # bathymetry trimmed in 'R/GA_coast_trim.R'
# colnames(bathdf)[3] <- "Depth"
st_crs(aus)         <- st_crs(aumpa)
st_crs(dirkh)       <- st_crs(aumpa) 

## get data locations /limits that need from MPA
##### SLA ####
sla.data <- readRDS(paste0("data/spatial/oceanography/", Zone, "_SLA_month.rds"))%>%
  ungroup()%>%
  dplyr::mutate(month=month.name[month])%>%
  dplyr::mutate(month = forcats::fct_relevel(month,c("January","February","March","April","May",
                               "June","July","August","September","October",
                               "November","December")))%>%
  glimpse()

min_sla =round(min(min(sla.data$sla,na.rm = TRUE), na.rm = TRUE),digits = 2)
max_sla= round(max(max(sla.data$sla,na.rm = TRUE), na.rm = TRUE), digits = 2)

title_legend <- "SLA"
p_1 <- ggplot() +
  geom_tile(data = sla.data%>%filter(month%in%c("January","March","May","July",
                                                "September","November")), 
            aes(x = Lon, y = Lat, fill = sla))+#, interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sla, to = max_sla, by = 0.02),
                       limits = c(min_sla, max_sla)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))+
  facet_wrap(~month, nrow = 4, ncol = 3)

png(filename = paste0("figures/spatial/", Zone, "_SLA_monthly_spatial.png"), 
    units = "in", res = 300, width = 6, height = 6.75)
p_1
dev.off()


######### SST #########

sst.data <- readRDS(paste0("data/spatial/oceanography/", Zone, "_SST_month.rds"))%>%
  ungroup()%>%
  dplyr::mutate(month=month.name[month])%>%
  dplyr::mutate(month = forcats::fct_relevel(month,c("January","February","March","April","May",
                                                     "June","July","August","September","October",
                                                     "November","December")))%>%
  glimpse()

min_sst =round(min(min(sst.data$sst,na.rm = TRUE), na.rm = TRUE))
max_sst= round(max(max(sst.data$sst,na.rm = TRUE), na.rm = TRUE))

title_legend <- "SST"
p_2 <- ggplot() +
  geom_tile(data = sst.data%>%filter(month%in%c("January","March","May","July",
                                                "September","November")), 
            aes(x = Lon, y = Lat, fill = sst))+#, interpolate = TRUE) + 
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = min_sst, to = max_sst, by = 1),
                       limits = c(min_sst, max_sst)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  # geom_quiver(data = curr_month, aes(x=long,y=lat,u=uu,v=vv), 
  #             vecsize=arrow_size, color = "white")+
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  # ggtitle(month.name[[i]])+
  scale_x_continuous(breaks=c(113.5,114.0,114.5))+
  facet_wrap(~month, nrow = 4, ncol = 3)
png(filename = paste0("figures/spatial/", Zone, "_SST_monthly_spatial.png"), 
    units = "in", res = 300, width = 6, height = 6.75)
p_2
dev.off()



##### DEGREE HEATING WEEKS ####
dhw.heatwave <- readRDS(paste0("data/spatial/oceanography/", Zone, "_DHW_heatwave.rds"))%>%
  ungroup() %>%
  dplyr::mutate(title=ifelse(year=='2011',"2011 March",year))%>%
  dplyr::mutate(title=ifelse(title=='2013',"2013 March",title))%>%
  glimpse()

min_dhw = round(min(min(dhw.heatwave$dhw,na.rm = TRUE), na.rm = TRUE))
max_dhw = round(max(max(dhw.heatwave$dhw,na.rm = TRUE), na.rm = TRUE))

title_legend <- "DHW"
p_3 <- ggplot() +
  geom_tile(data = dhw.heatwave, 
            aes(x = Lon, y = Lat, fill = dhw))+
  scale_fill_gradientn(colours = viridis(5),na.value = NA,
                       breaks = seq(from = 0, to = max_dhw, by = 5),
                       limits = c(0, max_dhw)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa,fill = NA, color = alpha("grey",0.5))+
  geom_sf(data = wampa,fill = NA, color = alpha("grey",0.5))+
  labs(x = "Longitude", y = "Latitude", fill = title_legend) +
  coord_sf(xlim = xxlim, ylim = yylim) +
  theme_minimal()+
  scale_x_continuous(breaks=c(113.0,114.0,115.0))+
  facet_wrap(~title)

png(filename = paste0("figures/spatial/", Zone, "_DHW_monthly_spatial.png"), 
    units = "in", res = 300, width = 6, height = 6)
p_3
dev.off()


##### ACIDIFICATION #####
acd_ts_monthly <- readRDS(paste0("data/spatial/oceanography/", Zone, "_acidification.rds")) %>%
  dplyr::filter(!year %in% c(1870, 2013)) %>%
  glimpse()

legend_title = "Season"
acd_mean_plot <- ggplot(data = acd_ts_monthly, aes(x = year, y = acd_mean)) + 
  geom_line() +
  geom_ribbon(aes(ymin = acd_mean-acd_sd, ymax = acd_mean+acd_sd), fill = "black",alpha = 0.15) +
  theme_classic() +
  labs(x = "Year", y = "pH")
  # scale_x_continuous(limits = c(1993,2022))
acd_mean_plot #plot with the other time series

##### Average plots - time series ####
#plot for sla, summer and winter mean
sla.monthly <- readRDS(paste0("data/spatial/oceanography/", Zone, "_SLA_ts.rds"))%>%
  dplyr::mutate(season = case_when(month %in% c(6,7,8) ~ "Winter", 
                                   month %in% c(12,1,2) ~ "Summer", 
                                  month %in% c(3,4,5) ~ "Autumn", 
                                  month %in% c(9,10,11) ~ "Spring" )) %>%
  dplyr::group_by(year, season) %>%
  dplyr::summarise(sla_mean_sea = mean(sla, na.rm = TRUE), sla_sd_sea = mean(sd, na.rm = TRUE)) %>%
  glimpse()

sla_plot <- sla.monthly %>% filter(grepl('Winter|Summer', season))

sla_mean_plot <- ggplot() + 
  geom_line(data = sla_plot, aes(x = year, y = sla_mean_sea, color = season)) + 
  geom_ribbon(data = sla_plot,aes(x = year, y = sla_mean_sea,
                                  ymin = sla_mean_sea-sla_sd_sea, 
                                  ymax = sla_mean_sea+sla_sd_sea, fill = season), 
              alpha = 0.2, show.legend = F) +
  theme_classic() +
  labs(x = "Year", y = "SLA (m)", color = legend_title)+
  scale_color_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))+
  scale_fill_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))+
  scale_x_continuous(limits = c(1993,2022))
sla_mean_plot

#plot for sst summer and winter mean
sst_tss <- readRDS(paste0("data/spatial/oceanography/", Zone, "_SST_ts.rds"))%>%
  dplyr::mutate(season = case_when(month %in% c(6,7,8) ~ "Winter", month %in% c(12,1,2) ~ "Summer", 
                                   month %in% c(3,4,5) ~ "Autumn", month %in% c(9,10,11) ~ "Spring" )) %>%
  group_by(year, season) %>%
  summarise(sst_mean = mean(sst, na.rm = TRUE),sd_sst = mean(sd, na.rm = TRUE)) %>%
  glimpse()

sst_plot <- sst_tss %>% filter(grepl('Winter|Summer', season))

#plot for sst, summer and winter mean
sst_mean_plot <- ggplot() + 
  geom_line(data = sst_plot, aes(x = year, y = sst_mean, color = season)) + 
  geom_ribbon(data = sst_plot,aes(x = year, y = sst_mean,
                                  ymin = sst_mean-sd_sst, 
                                  ymax = sst_mean+sd_sst, fill = season), 
              alpha = 0.2, show.legend = F) +
  theme_classic() +
  labs(x = "Year", y = expression(paste("SST (",degree~C,")")), color = legend_title)+
  scale_color_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))+
  scale_fill_manual(labels = c("Summer","Winter"), values = c("#e1ad68","#256b61"))+
  scale_x_continuous(limits = c(1993,2022))
sst_mean_plot

#plot for dhw data 
dhw_plot <- readRDS(paste0("data/spatial/oceanography/", Zone, "_DHW_ts.rds")) %>%
  group_by(year) %>%
  summarise(dhw_mean = mean(dhw, na.rm = TRUE),sd_dhw = mean(sd, na.rm = TRUE)) %>%
  glimpse()

#plot for dhw monthly
dhw_mean_plot <- ggplot() + 
  geom_vline(xintercept = 2011, color = "red", linetype = 5, alpha = 0.5)+
  geom_vline(xintercept = 2013, color = "red", linetype = 5, alpha = 0.5)+
  geom_line(data = dhw_plot, aes(x = year, y = dhw_mean)) + 
  geom_ribbon(data = dhw_plot,aes(x = year, y = dhw_mean,
                                  ymin = dhw_mean-sd_dhw, 
                                  ymax = dhw_mean+sd_dhw), 
              alpha = 0.2, show.legend = F) +
  theme_classic() +
  scale_x_continuous(limits = c(1993,2022))+
  labs(x = "Year", y = "DHW")
dhw_mean_plot

acd_mean_plot+sla_mean_plot+sst_mean_plot + dhw_mean_plot+plot_layout(ncol = 1, nrow = 4)

ggsave(paste0("figures/spatial/", Zone, "_acd_sla_sst_ts.png"), dpi = 300, width = 6, height = 6.75)
