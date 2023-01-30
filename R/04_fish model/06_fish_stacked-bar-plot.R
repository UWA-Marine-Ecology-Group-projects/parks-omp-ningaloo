###
# Project: Parks OMP Ningaloo
# Data:    BRUV fish, habitat
# Task:    Plotting 10 most abundant species w/ cute pics
# author:  Claude
# date:    November 2022
##

# Set directories----
rm(list=ls())

# Libraries required
library(GlobalArchive)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(rgdal)
library(raster)
library(png)
library(cowplot)

name <- "Parks-Ningaloo-synthesis"  # set study name

#OR set manually once
theme_collapse<-theme(      
  panel.grid.major=element_line(colour = "white"), 
  panel.grid.minor=element_line(colour = "white", size = 0.25), 
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

theme.larger.text<-theme(
  strip.text.x = element_text(size = 5,angle = 0),
  strip.text.y = element_text(size = 5),
  axis.title.x=element_text(vjust=-0.0, size=10),
  axis.title.y=element_text(vjust=0.0,size=10),
  axis.text.x=element_text(size=8),
  axis.text.y=element_text(size=8),
  legend.title = element_text(family="TN",size=8),
  legend.text = element_text(family="TN",size=8))


# read in maxn
maxn <- read.csv(paste0("data/tidy/", name, ".checked.maxn.csv")) %>%
  dplyr::mutate(sample = as.character(sample)) %>%
  glimpse()

preds <- readRDS(paste0("data/tidy/", name, "_nesp-habitat-bathy-derivatives.rds")) %>%
  dplyr::select(campaignid, sample, Z) %>%
  glimpse()

maxn <- maxn %>%
  left_join(preds) %>%
  glimpse()

maxn.meso <- maxn %>%
  dplyr::filter(Z >= -70)  %>%
  glimpse()

samps.meso <- maxn.meso %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(n = n())

maxn.raro <- maxn %>%
  dplyr::filter(Z < -70) %>%
  glimpse()

samps.raro <- maxn.raro %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(n = n())

# workout total maxn for each species ---
# Mesophotic (30 - 70m)
maxn.10.meso <- maxn.meso %>%
  dplyr::mutate(species = ifelse(genus %in% "Gymnocranius" & species %in% c("grandoculis", "griseus"), "spp", species)) %>%
  dplyr::mutate(species = ifelse(genus %in% "Pristipomoides" & species %in% c("filamentosus", "typus"), "spp", species)) %>%
  dplyr::mutate(species = ifelse(genus %in% "Gymnocranius" & species %in% c("sp1"), "spp", species)) %>%
  dplyr::mutate(species = ifelse(genus %in% "Pristipomoides" & species %in% c("sp1"), "spp", species)) %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  dplyr::filter(!scientific %in% c('Unknown spp', 'SUS sus'))%>%
  top_n(10)%>%
  glimpse()

# Rariphotic (70 - 200m)
maxn.10.raro <- maxn.raro %>%
  dplyr::mutate(species = ifelse(genus %in% "Gymnocranius" & species %in% c("grandoculis", "griseus"), "spp", species)) %>%
  dplyr::mutate(species = ifelse(genus %in% "Pristipomoides" & species %in% c("filamentosus", "typus"), "spp", species)) %>%
  dplyr::mutate(species = ifelse(genus %in% "Gymnocranius" & species %in% c("sp1"), "spp", species)) %>%
  dplyr::mutate(species = ifelse(genus %in% "Pristipomoides" & species %in% c("sp1"), "spp", species)) %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  dplyr::filter(!scientific %in% c('Unknown spp', 'SUS sus'))%>%
  top_n(10)%>%
  glimpse()

## Total frequency of occurance 
bar.10.meso <- ggplot(maxn.10.meso, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  #scale_x_discrete(limits = rev(levels(scientific)))+
  #annotation_custom(lcpic, xmin=0.5, xmax=2.5, ymin=.75, ymax=1.5)+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar.10.meso

bar.10.raro <- ggplot(maxn.10.raro, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  #scale_x_discrete(limits = rev(levels(scientific)))+
  #annotation_custom(lcpic, xmin=0.5, xmax=2.5, ymin=.75, ymax=1.5)+ 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar.10.raro

# Rariphotic 
# Load fish pictures for plotting ----
# 1. Decapterus spp
d.spp <- readPNG("data/images/Decapterus_spp_nb_TAYLOR.png") %>%
  as.raster()

# 2. Pristipomoides multidens
p.m <- readPNG("data/images/Pristipomoides multidens 300dpi.png") %>%
  as.raster()

# 3. Gymnocranius sp1.
g.g <- readPNG("data/images/Gymnocranius_grandoculis_nb_TAYLOR.png") %>%
  as.raster()

# 4. Lethrinus miniatus
l.m <- readPNG("data/images/Lethrinus miniatus 3cm.png") %>%
  as.raster()

# 5. Lethrinus rubrioperculatus
l.r <- readPNG("data/images/Lethrinidae-Dark.png") %>%
  as.raster()

# 6. Pristipomoides sp1.
p.t <- readPNG("data/images/Pristipomoides typus 3cm.png") %>%
  as.raster()

# 7. Carangoides chrysophrys
c.c <- readPNG("data/images/Carangoides_chrysophrys_nb_BORNT.png") %>%
  as.raster()

# 8. Naso hexacanthus
n.h <- readPNG("data/images/Acanthurus grammoptilus-3cmL.png") %>%
  as.raster()

# 9. Pentapodus nagasakiensis
p.n <- readPNG("data/images/Pentapodus porosus-3cmL.png") %>%
  as.raster()

# 10. Carangoides fulvoguttatus
c.f <- readPNG("data/images/Carangoides fulvoguttatus-3cmL.png") %>%
  as.raster()

## Top ten plot ----
bar.top.10.raro <-ggplot(maxn.10.raro %>% mutate(scientific = str_replace_all(.$scientific,
  c("fulvoguttatus"="fulvoguttatus*", "gymnostethus"="gymnostethus*",
    "chrysophrys"="chrysophrys*", "grandoculis"="grandoculis*",
    "rubrioperculatus"="rubrioperculatus*","miniatus"="miniatus*",
    "multidens"="multidens*", "Gymnocranius spp" = "Gymnocranius spp*",
    "Pristipomoides spp" = "Pristipomoides spp*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  # ylim (0, 1150)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  labs(title = "Rariphotic assemblage (70-200m)") +
  theme_bw() +
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse +
  theme.larger.text +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000), limits = c(0, 1150)) +
  annotation_raster(d.spp, xmin = 9.65, xmax = 10.35, ymin = 995, ymax = 995 + 210)+
  annotation_raster(p.m, xmin = 8.5,xmax = 9.5,ymin = 345, ymax = 345 + 350)+
  annotation_raster(g.g, xmin = 7.6, xmax = 8.4, ymin = 339 + 5, ymax = 339 + 270)+
  annotation_raster(l.m, xmin = 6.6, xmax = 7.4, ymin = 268 + 5, ymax = 268 + 255)+
  annotation_raster(l.r, xmin = 5.65, xmax = 6.35, ymin = 230 + 10, ymax = 230 + 270)+
  annotation_raster(p.t, xmin = 4.6, xmax = 5.4, ymin = 213 + 5, ymax = 213 + 290)+
  annotation_raster(c.c, xmin = 3.6, xmax = 4.4, ymin = 195 + 5, ymax = 195 + 250)+
  annotation_raster(n.h, xmin = 2.7, xmax = 3.3, ymin = 175 + 5, ymax = 175 + 290)+
  annotation_raster(p.n, xmin = 1.75, xmax = 2.25, ymin = 169 + 5, ymax = 169 + 150)+
  annotation_raster(c.f, xmin = 0.6, xmax = 1.4, ymin = 150 + 5, ymax = 150 + 290) +
  annotate(geom = "text", x = 1, y = 1000, label = "n = 167", fontface = "italic")
bar.top.10.raro

#save out plot
ggsave(paste0("figures/fish/", name, "_rariphotic.stacked.bar.plot.png"), bar.top.10.raro, dpi = 600, width = 7, height = 8)

# Mesophotic 
# Load fish pictures for plotting ----
# 1. Decapterus spp
d.spp <- readPNG("data/images/Decapterus_spp_nb_TAYLOR.png") %>%
  as.raster()

# 2. Lutjanus vitta
l.v <- readPNG("data/images/Lutjanus vitta 300dpi_nb.png") %>%
  as.raster()

# 3. Carangoides gymnostethus
c.g <- readPNG("data/images/Carangoides gymnostethus 3cm.png") %>%
  as.raster()

# 4. Lethrinus punctulatus
l.p <- readPNG("data/images/Lethrinus punctulatus-3cmL.png") %>%
  as.raster()

# 5. Gnathanodon speciosus
g.s <- readPNG("data/images/Gnathanodon speciosus-3cmL.png") %>%
  as.raster()

# 6. Gymnocranius grandoculis
g.g <- readPNG("data/images/Gymnocranius_grandoculis_nb_TAYLOR.png") %>%
  as.raster()

# 7. Lutjanus sebae
l.s <- readPNG("data/images/Lutjanus sebae 300dpi.png") %>%
  as.raster()

# 8. Lethrinus rubrioperculatus
l.r <- readPNG("data/images/Lethrinidae-Dark.png") %>%
  as.raster()

# 9. Lagocephalus sceleratus
l.sc <- readPNG("data/images/Lagocephalus_sceleratus_nb_TAYLOR.png") %>%
  as.raster()

# 10. Epinephelus areolatus
e.a <- readPNG("data/images/epinephelus_areolatus_nb.png") %>%
  as.raster()

## Top ten plot ----
bar.top.10.meso <-ggplot(maxn.10.meso %>% mutate(scientific = str_replace_all(.$scientific,
                                                                   c("vitta"="vitta*", "gymnostethus"="gymnostethus*",
                                                                     "speciosus" = "speciosus*",
                                                                     "punctulatus" = "punctulatus*", "sebae" = "sebae*",
                                                                     "chrysophrys"="chrysophrys*", "grandoculis"="grandoculis*",
                                                                     "rubrioperculatus"="rubrioperculatus*",
                                                                     "areolatus" = "areolatus*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 110)+
  labs(title = "Mesophotic assemblage (30-70m)") +
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(d.spp, xmin = 9.7, xmax = 10.3, ymin = 93, ymax = 93 + 17)+
  annotation_raster(l.v, xmin = 8.65,xmax = 9.35,ymin = 66, ymax = 66 + 20)+
  annotation_raster(c.g, xmin = 7.6, xmax = 8.4, ymin = 31 + 1, ymax = 31 + 33)+
  annotation_raster(l.p, xmin = 6.7, xmax = 7.3, ymin = 28 + 1, ymax = 28 + 20)+
  annotation_raster(g.s, xmin = 5.5, xmax = 6.5, ymin = 27 + 1, ymax = 27 + 36)+
  annotation_raster(g.g, xmin = 4.65, xmax = 5.35, ymin = 26, ymax = 26 + 20)+
  annotation_raster(l.s, xmin = 3.55, xmax = 4.45, ymin = 25, ymax = 25 + 25)+
  annotation_raster(l.r, xmin = 2.75, xmax = 3.25, ymin = 21 + 1, ymax = 21 + 20)+
  annotation_raster(l.sc, xmin = 1.8, xmax = 2.2, ymin = 21, ymax = 21 + 30)+
  annotation_raster(e.a, xmin = 0.65, xmax = 1.35, ymin = 16, ymax = 16 + 20) +
  annotate(geom = "text", x = 1, y = 90, label = "n = 14", fontface = "italic")
# bar.top.10.meso

#save out plot
ggsave(paste0("figures/fish/", name, "_mesophotic.stacked.bar.plot.png"), bar.top.10.meso, dpi = 600, width = 7, height = 8)


#Recreationally targeted species
#targeted species top 10 abundance
# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('NW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name,minlegal.wa)%>% 
  distinct()%>%
  glimpse()

fished.meso <- maxn.meso %>%
  dplyr::mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in% c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp",
                                                        "Lethrinidae Gymnocranius spp","Lethrinidae Lethrinus spp",
                                                        "Lethrinidae Unknown spp","Platycephalidae Platycephalus spp", 
                                                        "Lutjanidae Pristipomoides spp", "Lutjanidae Pristipomoides sp1",
                                                        "Lethrinidae Gymnocranius sp1")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius sp1"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae", 
                             "Carcharhinidae", "Sphyrnidae", "Pomacanthidae"))%>%    # Remove non-targeted families   
  dplyr::mutate(minlegal.wa = as.double(minlegal.wa)) %>%
  glimpse()

fished.raro <- maxn.raro %>%
  dplyr::mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in% c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp",
                                                        "Lethrinidae Gymnocranius spp","Lethrinidae Lethrinus spp",
                                                        "Lethrinidae Unknown spp","Platycephalidae Platycephalus spp", 
                                                        "Lutjanidae Pristipomoides spp", "Lutjanidae Pristipomoides sp1",
                                                        "Lethrinidae Gymnocranius sp1")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius sp1"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae", 
                             "Carcharhinidae", "Sphyrnidae", "Pomacanthidae"))%>%    # Remove non-targeted families   
  dplyr::mutate(minlegal.wa = as.double(minlegal.wa)) %>%
  glimpse()

# workout total maxn for each species ---
maxn.fished.meso <- fished.meso %>%
  mutate(scientific = paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn)) %>%
  # dplyr::filter(!scientific%in%"Lethrinus spp")%>%
  ungroup()%>%
  top_n(10)%>%
  glimpse()

maxn.fished.raro <- fished.raro %>%
  dplyr::mutate(species = ifelse(genus %in% "Gymnocranius" & species %in% c("grandoculis", "griseus"), "sp1", species)) %>%
  dplyr::mutate(species = ifelse(genus %in% "Pristipomoides" & species %in% c("filamentosus", "typus"), "sp1", species)) %>%
  mutate(scientific = paste(genus,species,sep=" ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn=sum(maxn)) %>%
  # dplyr::filter(!scientific%in%"Lethrinus spp")%>%
  ungroup()%>%
  top_n(10)%>%
  glimpse()

#have a look
bar.fished.meso <- ggplot(maxn.fished.meso, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar.fished.meso

bar.fished.raro <- ggplot(maxn.fished.raro, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar.fished.raro

# Mesophotic
# Load fish pics
# 1. Lutjanus vitta
# Already loaded

# 2. Lethrinus punctulatus
# Already loaded

# 3. Gymnocranius grandoculis
# Already loaded

# 4. Lutjanus sebae
# Already loaded

# 5. Gymnocranius griseus
# Already loaded - use grandoculis

# 6. Lethrinus nebulosus
l.n <- readPNG("data/images/lethrinus nebulosus 3cm.png") %>%
  as.raster()

# 7. Pristipomoides multidens
# Already loaded

# 8. Lethrinus ravus
# Already loaded - use lethrinus spp

# 9. Argyrops spinifer
a.s <- readPNG("data/images/Argyrops spinifer - bw.png") %>%
  as.raster()

# 10. Choerodon cauteroma
c.c <- readPNG("data/images/Choerodon cauteroma-3cmL.png") %>%
  as.raster()

## Top ten plot ----
bar.fished.meso <-ggplot(maxn.fished.meso %>% mutate(scientific = str_replace_all(.$scientific, 
                                                                                               c("spinifer" = "bleekeri"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 80)+
  labs(title = "Mesophotic assemblage (30-70m)") +
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(l.v, xmin = 9.7, xmax = 10.3, ymin = 66, ymax = 66 + 13)+
  annotation_raster(l.p, xmin = 8.65,xmax = 9.35,ymin = 28 + 1, ymax = 28 + 20)+
  annotation_raster(g.g, xmin = 7.65, xmax = 8.35, ymin = 26 + 1, ymax = 26 + 18)+
  annotation_raster(l.s, xmin = 6.55, xmax = 7.45, ymin = 25 + 1, ymax = 25 + 22)+
  annotation_raster(g.g, xmin = 5.7, xmax = 6.3, ymin = 8 + 1, ymax = 8 + 15)+
  annotation_raster(l.n, xmin = 4.6, xmax = 5.4, ymin = 6 + 1, ymax = 6 + 25)+
  annotation_raster(p.m, xmin = 3.55, xmax = 4.45, ymin = 5, ymax = 5 + 30)+
  annotation_raster(l.r, xmin = 2.75, xmax = 3.25, ymin = 5 + 1, ymax = 5 + 15)+
  annotation_raster(a.s, xmin = 1.7, xmax = 2.3, ymin = 4 + 1, ymax = 4 + 13)+
  annotation_raster(c.c, xmin = 0.65, xmax = 1.35, ymin = 3 + 1, ymax = 3 + 20) +
  annotate(geom = "text", x = 1, y = 60, label = "n = 14", fontface = "italic")
# bar.fished.meso

#save out plot
ggsave(paste0("figures/fish/", name, "_mesophotic.fished..stacked.bar.plot.png"), bar.fished.meso, dpi = 600, width = 7, height = 8)

# Rariphotic
# Load fish pics
# 1. Pristipomoides multidens
# Already loaded

# 2. Gymnocranius sp1.
# Already loaded - use grandoculis

# 3. Lethrinus miniatus
# Already loaded

# 4. Pristipomoides sp1
# Already loaded - use typus

# 5. Argyrops spinifer/bleekeri
# Already loaded

# 6. Lutjanus sebae
# Already loaded

# 7. Lethrinus ravus - use lethrinus spp
# Already loaded

# 8. Lethrinus nebulosus
# Already loaded 

# 9. Lethrinus punctulatus
# Already loaded

# 10. Dentex carpenteri
d.c <- readPNG("data/images/Dentex_carpenteri_nb_GIBBONS.png") %>%
  as.raster()


## Top ten plot ----
bar.fished.raro<-ggplot(maxn.fished.raro %>% mutate(scientific = str_replace_all(.$scientific, 
                                                                                  c("spinifer" = "bleekeri"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 425)+
  labs(title = "Rariphotic assemblage (70-200m)") +
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(p.m, xmin = 9.6, xmax = 10.4, ymin = 345, ymax = 345 + 100)+
  annotation_raster(g.g, xmin = 8.65,xmax = 9.35,ymin = 339 + 1, ymax = 339 + 75)+
  annotation_raster(l.m, xmin = 7.55, xmax = 8.45, ymin = 268 + 1, ymax = 268 + 100)+
  annotation_raster(p.t, xmin = 6.65, xmax = 7.35, ymin = 213 + 1, ymax = 213 + 100)+
  annotation_raster(a.s, xmin = 5.7, xmax = 6.3, ymin = 136 + 1, ymax = 136 + 60)+
  annotation_raster(l.s, xmin = 4.6, xmax = 5.4, ymin = 89 + 1, ymax = 89 + 100)+
  annotation_raster(l.r, xmin = 3.65, xmax = 4.35, ymin = 75+1, ymax = 75 + 100)+
  annotation_raster(l.n, xmin = 2.6, xmax = 3.4, ymin = 41 + 1, ymax = 41 + 100)+
  annotation_raster(l.p, xmin = 1.7, xmax = 2.3, ymin = 19 + 1, ymax = 19 + 75)+
  annotation_raster(d.c, xmin = 0.65, xmax = 1.35, ymin = 19 + 1, ymax = 19 + 65) +
  annotate(geom = "text", x = 1, y = 300, label = "n = 167", fontface = "italic")
bar.fished.raro

#save out plot
ggsave(paste0("figures/fish/", name, "_rariphotic.fished.stacked.bar.plot.png"), bar.fished.raro, dpi = 600, width = 7, height = 8)





