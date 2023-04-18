###
# Project: Parks OMP Ningaloo
# Data:    BRUVS, BOSS
# Task:    Format tidy habitat and relief data
# author:  Claude Spencer
# date:    October 2022
##

# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive") # Run once
library(GlobalArchive)

# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Study name ----
study <- "Parks-Ningaloo-synthesis" 

## Set your working directory ----
working.dir <- getwd() # this only works through github projects

## Save these directory names to use later----
data.dir <- paste(working.dir,"data",sep="/") 
raw.dir <- paste(data.dir,"raw",sep="/") 
tm.export.dir <- paste(raw.dir,"TM Export",sep="/") 
em.export.dir <- paste(raw.dir, "EM Export", sep = "/")

# Read in metadata----
read_files_csv <- function(flnm) {
  flnm %>%
    readr::read_csv(col_types = readr::cols(.default = "c")) %>%
    GlobalArchive::ga.clean.names() %>%
    dplyr::mutate(campaign.naming = str_replace_all(flnm, paste0(em.export.dir,"/"),"")) %>%
    tidyr::separate(campaign.naming,into = c("campaignid"), sep="/", extra = "drop", fill = "right") %>%
    dplyr::mutate(campaignid=str_replace_all(.$campaignid,c("_Metadata.csv"= "")))
}


metadata <- list.files(path = em.export.dir, 
                       recursive = T,
                       pattern = "_Metadata.csv",
                       full.names = T)  %>% # read in the file
  purrr::map_dfr(~read_files_csv(.)) %>%
  dplyr::select(campaignid, sample, latitude, longitude, date, site, location, successful.count, depth) %>% # select only these columns to keep
  mutate(sample = as.character(sample)) %>% # in this example dataset, the samples are numerical
  dplyr::mutate(boss.method = ifelse(str_detect(campaignid, "Naked"), "Naked", NA)) %>%
  dplyr::mutate(boss.method = ifelse(str_detect(campaignid, "Squid"), "Squid", boss.method), # Not even sure why we need this
                campaignid = ifelse(campaignid %in% c("2022-05_PtCloates_Naked-BOSS",
                                                      "2022-05_PtCloates_Squid-BOSS"),
                                    "2022-05_PtCloates_BOSS", campaignid)) %>%
  dplyr::filter(campaignid %in% c("2019-08_Ningaloo_stereo-BRUVs",
                                   "2021-05_PtCloates_BOSS",
                                   "2021-05_PtCloates_stereo-BRUVS",
                                  "2022-05_PtCloates_stereo-BRUVS",
                                  "2022-05_PtCloates_BOSS")) %>%
  glimpse() # preview

unique(metadata$campaignid)
names(metadata)

# read in the points annotations ----
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
  glimpse() # preview

unique(points$sample)
unique(points$campaignid)
# errors <- list.files(path = tm.export.dir,
#                      recursive = T,
#                      pattern = "Dot Point Measurements.txt",
#                      full.names = T) %>%
#   purrr::map_dfr(~read_tm_delim(.)) %>% # read in the file
#   dplyr::filter(relief.file %in% "No") %>%
#   mutate(newbroad = ifelse(BROAD %in% c("", "NA", " ", NA, NULL), Broad, BROAD),
#          newmorphology = ifelse(BROAD %in% c("", "NA", " ", NA, NULL), Morphology, MORPHOLOGY),
#          newtype = ifelse(BROAD %in% c("", "NA", " ", NA, NULL), Type, TYPE)) %>%
#   dplyr::select(-c(Broad, BROAD ,Morphology, MORPHOLOGY,Type, TYPE, RELIEF)) %>%
#   ga.clean.names() %>% # tidy the column names using GlobalArchive function
#   mutate(sample = str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
#   mutate(sample = str_replace_all(.$sample,c("_.*"=""))) %>%                    # For files with the new naming convention - but no frame information fields added
#   mutate(sample = as.character(sample)) %>%
#   dplyr::rename(broad = newbroad,
#                 morphology = newmorphology,
#                 type = newtype) %>%
#   dplyr::filter(is.na(broad)) %>%
#   dplyr::select(campaignid, filename, sample, direction, broad) %>%
#   glimpse()
# 
# write.csv(errors, file = "data/errors to check/Parks-Ningaloo-synthesis_habitat-points-missed.csv",
#           row.names = F) # Errors to fix but will continue on

# test <- points %>% dplyr::filter(is.na(broad) | broad %in% c("", " "))

unique(points$campaignid)
length(unique(points$sample)) # 197 samples

no.annotations <- points %>%
  group_by(campaignid, sample) %>%
  dplyr::summarise(points.annotated=n()) # Looks ok - 1 with extras, couple boss with only 3 directions

relief <- list.files(path = tm.export.dir,
                     recursive = T,
                     pattern = "Dot Point Measurements.txt",
                     full.names = T) %>%
  purrr::map_dfr(~read_tm_delim(.)) %>% # read in the file
  dplyr::select(campaignid, Filename, Image.row, Image.col, Relief, RELIEF, relief.file) %>%
  dplyr::mutate(Relief = ifelse(is.na(Relief), RELIEF, Relief)) %>%
  dplyr::select(-RELIEF) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample = gsub("_.*", "", sample)) %>%  
  mutate(sample = as.character(sample)) %>%
  dplyr::filter(relief.file %in% "Yes" | campaignid %in% "2019-08_Ningaloo_stereo-BRUVs") %>%
  dplyr::select(-filename) %>%
  # dplyr::filter(!is.na(relief)) %>%
  glimpse() # preview

unique(relief$sample)
unique(relief$campaignid)

test1 <- relief %>% dplyr::filter(is.na(relief)) # 0 - thank the lord

length(unique(relief$sample)) # 240 samples - why does this not match?

no.annotations <- relief%>%
  group_by(campaignid, sample)%>%
  dplyr::summarise(relief.annotated=n()) # Looks good

habitat <- bind_rows(points, relief)
unique(habitat$sample)
test2 <- habitat %>%
  dplyr::filter(is.na(broad) & is.na(relief)) # These 11 are missing from the first test

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, by = c("campaignid","sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat, by = c("campaignid","sample")) # samples in the metadata that don't have a match in habitat
# 1 missing from the old campaign - no point fixing

# CREATE catami_broad------
broad.points <- habitat %>%
  dplyr::mutate(id = 1:nrow(habitat)) %>% # Key for tidyr::spread
  dplyr::select(-c(morphology,type,relief, image.row, 
                   image.col, fieldofview, relief.file))%>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(broad=paste("broad",broad,sep = ".")) %>%
  dplyr::mutate(count=1) %>%
  dplyr::group_by(campaignid, sample) %>%
  tidyr::spread(key=broad,value=count,fill=0) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,3:(ncol(.))],na.rm = TRUE )) %>%
  ga.clean.names() %>%
  glimpse

detailed.points <- habitat %>%
  dplyr::mutate(id = 1:nrow(habitat)) %>% # Key for tidyr::spread
  dplyr::select(-c(relief, image.row, 
                   image.col, fieldofview, relief.file))%>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(detailed = paste("broad",broad,morphology, type, sep = ".")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::select(-c(broad, morphology, type)) %>%
  dplyr::group_by(campaignid, sample) %>%
  tidyr::spread(key = detailed, value = count,fill = 0) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::select(-id) %>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,3:(ncol(.))],na.rm = TRUE )) %>%
  dplyr::mutate(`broad.Unconsolidated.Pebble / gravel (biogenic).` = `broad.Unconsolidated.Pebble / gravel (biogenic).` + `broad.Unconsolidated.Pebble.gravel (biogenic)`,
                `broad.Unconsolidated.Pebble / gravel (gravel 2-10mm).` = `broad.Unconsolidated.Pebble / gravel (gravel 2-10mm).` + `broad.Unconsolidated.Pebble.gravel (gravel 2-10mm)`,
                `broad.Unconsolidated.Sand / mud (coarse sand).` = `broad.Unconsolidated.Sand / mud (coarse sand).` + `broad.Unconsolidated.Sand.mud (coarse sand)`,
                `broad.Unconsolidated.Sand / mud (fine sand).` = `broad.Unconsolidated.Sand / mud (fine sand).` + `broad.Unconsolidated.Sand.mud (fine sand)`) %>%
  dplyr::select(-c(`broad.Unconsolidated.Pebble.gravel (biogenic)`,
                   `broad.Unconsolidated.Pebble.gravel (gravel 2-10mm)`,
                   `broad.Unconsolidated.Sand.mud (coarse sand)`,
                   `broad.Unconsolidated.Sand.mud (fine sand)`)) %>%
  ga.clean.names() %>%
  glimpse

# Create relief----
relief.grid <- habitat %>%
  dplyr::filter(!broad %in% c("Open Water","Unknown")) %>%
  dplyr::filter(!relief %in% c("",NA, "Unknown")) %>%
  dplyr::select(-c(broad,morphology,type,fieldofview,image.row,image.col, relief.file)) %>%
  dplyr::mutate(relief.rank=ifelse(relief %in% c(".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",
                                                 "0. Flat substrate, sandy, rubble with few features. ~0 substrate slope."),0,
                                   ifelse(relief %in% c(".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",
                                                        "1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope."),1,
                                          ifelse(relief %in% c(".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",
                                                               "2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope."),2,
                                                 ifelse(relief %in% c(".3. Good relief structure with some overhangs. >45 substrate slope.",
                                                                      "3. Good relief structure with some overhangs. >45 substrate slope."),3,
                                                        ifelse(relief %in% c(".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",
                                                                             "4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope."),4,
                                                               ifelse(relief %in% c(".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",
                                                                                    "5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope."),5,relief)))))))%>%
  dplyr::select(-c(relief))%>%
  dplyr::mutate(relief.rank=as.numeric(relief.rank))%>%
  dplyr::group_by(campaignid, sample)%>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  dplyr::ungroup()%>%
  glimpse()


# Write final habitat data----
habitat.broad.points <- metadata %>%
  left_join(broad.points, by = c("campaignid","sample"))%>%
  left_join(relief.grid) %>%
  dplyr::filter(!is.na(broad.octocoral.black)) %>%
  # dplyr::filter(!sample %in% "19.05") %>%
  glimpse()

write.csv(habitat.broad.points,file = paste0("data/tidy/",study,"_random-points_broad.habitat.csv"), 
          row.names=FALSE)
write.csv(detailed.points,file = paste0("data/staging/",study,"_random-points_detailed.habitat.csv"), 
          row.names=FALSE)
setwd(working.dir)
