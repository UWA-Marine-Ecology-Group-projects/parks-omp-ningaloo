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
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  dplyr::filter(!scientific %in% c('Unknown spp', 'SUS sus'))%>%
  top_n(10)%>%
  glimpse()

# Rariphotic (70 - 200m)
maxn.10.raro <- maxn.raro %>%
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

# Mesophotic 
# Load fish pictures for plotting ----
# 1. Decapterus spp
d.spp <- readPNG("data/images/Decapterus_spp_nb_TAYLOR.png") %>%
  as.raster()

# 2. Pristipomoides multidens
p.m <- readPNG("data/images/Pristipomoides multidens 300dpi.png") %>%
  as.raster()

# 3. Lethrinus miniatus
l.m <- readPNG("data/images/Lethrinus miniatus 3cm.png") %>%
  as.raster()

# 4. Lethrinus rubrioperculatus
l.r <- readPNG("data/images/Lethrinidae-Dark.png") %>%
  as.raster()

# 5. Carangoides chrysophrys
c.c <- readPNG("data/images/Carangoides_chrysophrys_nb_BORNT.png") %>%
  as.raster()

# 6. Naso hexacanthus
n.h <- readPNG("data/images/Acanthurus grammoptilus-3cmL.png") %>%
  as.raster()

# 7. Pentapodus nagasakiensis
p.n <- readPNG("data/images/Pentapodus porosus-3cmL.png") %>%
  as.raster()

# 8. Gymnocranius grandoculis
g.g <- readPNG("data/images/Gymnocranius_grandoculis_nb_TAYLOR.png") %>%
  as.raster()

# 9. Carangoides fulvoguttatus
c.f <- readPNG("data/images/Carangoides fulvoguttatus-3cmL.png") %>%
  as.raster()

# 10. Gymnocranius sp1
# Use Gymnocranius grandoculis image


## Top ten plot ----
bar.top.10.raro <-ggplot(maxn.10.raro %>% mutate(scientific = str_replace_all(.$scientific,
  c("fulvoguttatus"="fulvoguttatus*", "gymnostethus"="gymnostethus*",
    "chrysophrys"="chrysophrys*", "grandoculis"="grandoculis*",
    "rubrioperculatus"="rubrioperculatus*","miniatus"="miniatus*",
    "multidens"="multidens*", "sp1" = "sp1*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
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
  annotation_raster(l.m, xmin = 7.6, xmax = 8.4, ymin = 268 + 5, ymax = 268 + 270)+
  annotation_raster(l.r, xmin = 6.6, xmax = 7.4, ymin = 230 + 5, ymax = 230 + 255)+
  annotation_raster(c.c, xmin = 5.5, xmax = 6.5, ymin = 195 + 10, ymax = 195 + 270)+
  annotation_raster(n.h, xmin = 4.7, xmax = 5.3, ymin = 175 + 5, ymax = 175 + 290)+
  annotation_raster(p.n, xmin = 3.85, xmax = 4.15, ymin = 169, ymax = 169 + 150)+
  annotation_raster(g.g, xmin = 2.6, xmax = 3.4, ymin = 150 + 5, ymax = 150 + 290)+
  annotation_raster(c.f, xmin = 1.6, xmax = 2.4, ymin = 150 + 5, ymax = 150 + 290)+
  annotation_raster(g.g, xmin = 0.6, xmax = 1.4, ymin = 149 + 5, ymax = 149 + 290) +
  annotate(geom = "text", x = 1, y = 1000, label = "n = 167", fontface = "italic")
bar.top.10.raro

#save out plot
ggsave(paste0("figures/fish/", name, "_mesophotic.stacked.bar.plot.png"), bar.top.10.raro, dpi = 600, width = 7, height = 8)

# Rariophotic 
# Load fish pictures for plotting ----
# 1. Decapterus spp
d.spp <- readPNG("data/images/Decapterus_spp_nb_TAYLOR.png") %>%
  as.raster()

# 2. Pristipomoides multidens
p.m <- readPNG("data/images/Pristipomoides multidens 300dpi.png") %>%
  as.raster()

# 3. Lethrinus miniatus
l.m <- readPNG("data/images/Lethrinus miniatus 3cm.png") %>%
  as.raster()

# 4. Lethrinus rubrioperculatus
l.r <- readPNG("data/images/Lethrinidae-Dark.png") %>%
  as.raster()

# 5. Carangoides chrysophrys
c.c <- readPNG("data/images/Carangoides_chrysophrys_nb_BORNT.png") %>%
  as.raster()

# 6. Naso hexacanthus
n.h <- readPNG("data/images/Acanthurus grammoptilus-3cmL.png") %>%
  as.raster()

# 7. Gymnocranius grandoculis
g.g <- readPNG("data/images/Gymnocranius_grandoculis_nb_TAYLOR.png") %>%
  as.raster()

# 8. Pentapodus nagasakiensis
p.n <- readPNG("data/images/Pentapodus porosus-3cmL.png") %>%
  as.raster()

# 9. Carangoides gymnostethus
c.g <- readPNG("data/images/Carangoides gymnostethus 3cm.png") %>%
  as.raster()

# 10. Carangoides fulvoguttatus
c.f <- readPNG("data/images/Carangoides fulvoguttatus-3cmL.png") %>%
  as.raster()

## Top ten plot ----
bar.top.10<-ggplot(maxn.10 %>% mutate(scientific = str_replace_all(.$scientific,
                                                                   c("fulvoguttatus"="fulvoguttatus*", "gymnostethus"="gymnostethus*",
                                                                     "chrysophrys"="chrysophrys*", "grandoculis"="grandoculis*",
                                                                     "rubrioperculatus"="rubrioperculatus*","miniatus"="miniatus*",
                                                                     "multidens"="multidens*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 1250)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(d.spp, xmin = 9.65, xmax = 10.35, ymin = 1089, ymax = 1089 + 210)+
  annotation_raster(p.m, xmin = 8.5,xmax = 9.5,ymin = 361, ymax = 361 + 350)+
  annotation_raster(l.m, xmin = 7.6, xmax = 8.4, ymin = 269 + 5, ymax = 269 + 270)+
  annotation_raster(l.r, xmin = 6.6, xmax = 7.4, ymin = 251 + 5, ymax = 251 + 255)+
  annotation_raster(c.c, xmin = 5.5, xmax = 6.5, ymin = 205 + 10, ymax = 205 + 270)+
  annotation_raster(n.h, xmin = 4.65, xmax = 5.35, ymin = 179 + 5, ymax = 179 + 290)+
  annotation_raster(g.g, xmin = 3.55, xmax = 4.45, ymin = 179, ymax = 179 + 220)+
  annotation_raster(p.n, xmin = 2.85, xmax = 3.15, ymin = 171 + 5, ymax = 171 + 150)+
  annotation_raster(c.g, xmin = 1.6, xmax = 2.4, ymin = 161 + 5, ymax = 161 + 290)+
  annotation_raster(c.f, xmin = 0.6, xmax = 1.4, ymin = 157 + 5, ymax = 157 + 290)
# bar.top.10

#save out plot
ggsave(paste0("figures/fish/", name, "_rariophotic.stacked.bar.plot.png"), bar.top.10, dpi = 600, width = 7, height = 8)


#Recreationally targeted species
#targeted species top 10 abundance
# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  # filter(grepl('NW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name,minlegal.wa)%>% 
  distinct()%>%
  glimpse()

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp","Lethrinidae Gymnocranius spp",
                                                       "Lethrinidae Lethrinus spp","Lethrinidae Unknown spp","Platycephalidae Platycephalus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae"))%>%    # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::filter(!species%in%c("albimarginatus","longimanus"))%>%
  glimpse()

# workout total maxn for each species ---
maxn.fished.npz6<-fished.species %>%
  dplyr::filter(location%in%"NPZ6")%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  # dplyr::filter(!scientific%in%"Lethrinus spp")%>%
  ungroup()%>%
  top_n(10)%>%
  glimpse()

#have a look
bar.npz6<-ggplot(maxn.fished.npz6, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar.npz6


#load fish pics
#NPZ6
#1 Lethrinus miniatus
#already loaded

#2 Chrysophrys auratus
#already loaded

#3 Choerodon rubescens
#already loaded

#4 Lethrinus nebulosus
#already loaded

#5 Seriola hippos
s.h <- as.raster(readPNG("data/images/Seriola_hippos_nb_HQ_TAYLOR.png"))

#6 Scomberomorus spp
s.spp <- as.raster(readPNG("data/images/Scombridae-Dark.png"))

#7 Lethrinus spp
l.spp <- as.raster(readPNG("data/images/Lethrinidae-Dark.png"))

#8 Lethrinus ravus
#as above

#9 Epinephelus rivulatus
e.r <- as.raster(readPNG("data/images/Serranidae-Dark.png"))

#10 Carcharhinus plumbeus
c.p <- as.raster(readPNG("data/images/Carcharinus plumbeus 5cmL adapt.png"))



#plot final bar plot
#npz6
bar.fished.npz6<-ggplot(maxn.fished.npz6, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 110)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(l.m, xmin=9.6,xmax=10.4,ymin=90, ymax=115)+
  annotation_raster(c.au, xmin=8.6,xmax=9.4,ymin=80, ymax=105)+
  annotation_raster(c.r, xmin=7.65, xmax=8.35, ymin=50, ymax=75)+
  annotation_raster(l.n, xmin=6.55,xmax=7.45,ymin=28, ymax=55)+
  annotation_raster(s.h, xmin=5.55,xmax=6.45,ymin=20, ymax=50)+
  annotation_raster(s.spp, xmin=4.55,xmax=5.45,ymin=10, ymax=45)+
  annotation_raster(l.spp, xmin=3.7,xmax=4.3,ymin=7, ymax=25)+
  annotation_raster(l.spp, xmin=2.7,xmax=3.3,ymin=7, ymax=25)+
  annotation_raster(e.r, xmin=1.75,xmax=2.25,ymin=5, ymax=20)+
  annotation_raster(c.p, xmin=0.4,xmax=1.6,ymin=5, ymax=50)
# ggtitle("10 most abundant species") +
# theme(plot.title = element_text(hjust = 0))
bar.fished.npz6
  
#save out plot
ggsave("plots/fish/abundant.targets.npz6.png",bar.fished.npz6,dpi=600,width=6.0, height = 6.0)


