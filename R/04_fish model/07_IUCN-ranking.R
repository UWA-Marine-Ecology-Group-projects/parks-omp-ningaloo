#Claude has edited this and it is pretty poxy
#need to fix up

# Set directories----
rm(list=ls())

# Study name ----
name <- "Parks-Ningaloo-synthesis"  # set study name

# Libraries required
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(googlesheets4)
library(tidyverse)
library(GlobalArchive)

maxn <- read.csv(paste0("data/tidy/", name, ".complete.maxn.csv")) %>%
  glimpse()

species.list <- as.data.frame(unique(maxn$scientific))

num.fish <- maxn %>%
  summarise(fish = sum(maxn)) %>%
  glimpse() # 6171 individuals

num.drops <- maxn %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(n = n())

length(unique(maxn$scientific)) # 216 species
length(unique(maxn$genus)) # 108 genera
length(unique(maxn$family)) # 52 families

tm.export.dir <- "data/raw/TM Export"

read_tm_delim <- function(flnm) {
  read.delim(flnm,header = T,skip = 4,stringsAsFactors = FALSE, colClasses = "character") %>%
    dplyr::mutate(campaign.naming = str_replace_all(flnm, paste0(tm.export.dir,"/"),"")) %>%
    tidyr::separate(campaign.naming,into = c("campaignid"), sep="/", extra = "drop", fill = "right") %>%
    dplyr::mutate(relief.file = ifelse(str_detect(campaignid, "Relief"), "Yes", "No")) %>%
    dplyr::mutate(direction = ifelse(str_detect(campaignid, "Backwards"), "Backwards", "Forwards")) %>%
    dplyr::mutate(campaignid = str_replace_all(.$campaignid,c("_Backwards_Dot Point Measurements.txt"= "",
                                                              "_Forwards_Dot Point Measurements.txt"= "",
                                                              "_Backwards_Relief_Dot Point Measurements.txt" = "",
                                                              "_Forwards_Relief_Dot Point Measurements.txt" = "",
                                                              "_Relief_Dot Point Measurements.txt" = "",
                                                              "_Dot Point Measurements.txt"= "")))
}
  
points <- list.files(path = tm.export.dir,
                     recursive = T,
                     pattern = "Dot Point Measurements.txt",
                     full.names = T) %>%
  purrr::map_dfr(~read_tm_delim(.)) %>% # read in the file
  dplyr::filter(relief.file %in% "No") %>%
  mutate(newbroad = ifelse(BROAD %in% c("", "NA", " ", NA, NULL), Broad, BROAD),
         newmorphology = ifelse(BROAD %in% c("", "NA", " ", NA, NULL), Morphology, MORPHOLOGY),
         newtype = ifelse(BROAD %in% c("", "NA", " ", NA, NULL), Type, TYPE)) %>%
  dplyr::select(-c(Broad, BROAD ,Morphology, MORPHOLOGY,Type, TYPE, RELIEF)) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample = gsub("_.*", "", sample)) %>%                    # For files with the new naming convention - very confusing to try and use the new columns
  mutate(sample = as.character(sample)) %>%
  dplyr::mutate(sample = ifelse(campaignid %in% c("2022-05_PtCloates_BOSS"), 
                                period, sample)) %>%                            # Using the new frame information fields for the newer campaigns
  dplyr::rename(broad = newbroad,
                morphology = newmorphology,
                type = newtype) %>%
  dplyr::select(campaignid, sample,image.row,image.col,
                broad,morphology,type,fieldofview) %>%     # select only these columns to keep
  dplyr::distinct(broad, morphology, type) %>%
  glimpse() # preview

# Read in life history
# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('NW', marine.region)) %>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name,minlegal.wa)%>% 
  distinct()%>%
  glimpse()

fished.species <- maxn %>%
  # dplyr::mutate(scientific = paste(family, genus, species, sep = " ")) %>%
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
  dplyr::select(scientific, australian.common.name, fishing.type) %>%
  distinct() %>%
  glimpse()


iucn.species <- maxn %>%
  # dplyr::mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  dplyr::left_join(master) %>%
  dplyr::filter(!iucn.ranking %in% c(NA, "Least Concern", "Data Deficient")) %>%
  dplyr::select(scientific, australian.common.name, iucn.ranking) %>%
  distinct() %>%
  glimpse()

write.csv(fished.species, file = paste0("data/tidy/", 
                                        name, "_fished.species.table.csv"), 
          row.names = F)

write.csv(iucn.species, file = paste0("data/tidy/", 
                                        name, "_iucn.species.table.csv"), 
          row.names = F)

