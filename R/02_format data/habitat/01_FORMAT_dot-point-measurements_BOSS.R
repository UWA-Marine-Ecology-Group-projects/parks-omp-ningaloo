# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive")
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
tidy.dir <- paste(data.dir,"Tidy",sep="/")
tm.export.dir <- paste(raw.dir,"TM Export",sep="/") 
em.export.dir <- paste(raw.dir, "EM Export", sep = "/")
error.dir <- paste(raw.dir,"errors to check",sep="/") 

# Read in the metadata----
setwd(em.export.dir)
dir()

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
  dplyr::filter(!campaignid %in% c("2021-08_Yardie-Creek_Baited-BOSS",
                                   "2021-08_Yardie-Creek_Flasher-BOSS",
                                   "2021-08_Yardie-Creek_stereo-BRUVs",
                                   "2021-05_NingalooTransect_BOSS")) %>%
  dplyr::mutate(campaignid = ifelse(campaignid %in% ))
  glimpse() # preview

unique(metadata$campaignid)
names(metadata)

# Read in habitat ----
setwd(tm.export.dir)
dir()

# read in the points annotations ----
read_tm_delim <- function(flnm) {
   read.delim(flnm,header = T,skip = 4,stringsAsFactors = FALSE, colClasses = "character") %>%
    dplyr::mutate(campaign.naming = str_replace_all(flnm, paste0(tm.export.dir,"/"),"")) %>%
    tidyr::separate(campaign.naming,into = c("campaignid"), sep="/", extra = "drop", fill = "right") %>%
    dplyr::mutate(relief.file = ifelse(str_detect(campaignid, "Relief"), "Yes", "No")) %>%
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
  dplyr::select(-c(Broad, BROAD ,Morphology, MORPHOLOGY,Type, TYPE)) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample = as.character(sample)) %>%
  dplyr::filter(relief.file %in% "No") %>%
  dplyr::rename(broad = newbroad,
                morphology = newmorphology,
                type = newtype) %>%
  dplyr::select(campaignid, sample,image.row,image.col,
                broad,morphology,type,fieldofview) %>%     # select only these columns to keep
  glimpse() # preview


test <- points %>% dplyr::filter(is.na(broad) | broad %in% c("", " "))

unique(points$campaignid)
length(unique(points$sample)) # 179 samples

no.annotations <- points %>%
  group_by(campaignid, sample) %>%
  dplyr::summarise(points.annotated=n()) # Looks ok - 1 with extras, couple boss with only 3 directions

relief <- list.files(path = tm.export.dir,
                     recursive = T,
                     pattern = "Dot Point Measurements.txt",
                     full.names = T) %>%
  purrr::map_dfr(~read_tm_delim(.)) %>% # read in the file
  dplyr::select(campaignid, Filename, Image.row, Image.col, Relief, relief.file) %>%
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample = str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample = as.character(sample)) %>%
  dplyr::filter(relief.file %in% "Yes" | campaignid %in% "2019-08_Ningaloo-Deep_stereo-BRUVs") %>%
  # dplyr::filter(!is.na(relief)) %>%
  glimpse() # preview

test <- relief %>% dplyr::filter(is.na(relief)) # 0 - thank the lord

length(unique(relief$sample)) # 179 samples

no.annotations <- relief%>%
  group_by(campaignid, sample)%>%
  dplyr::summarise(relief.annotated=n()) # Looks good

habitat <- bind_rows(points, relief)

test <- habitat %>%
  dplyr::filter(is.na(broad) & is.na(relief)) # I think there are just loads from open water?

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat, by = c("sample")) # samples in the metadata that don't have a match in habitat

# Create %fov----
fov.points <- habitat%>%
  dplyr::select(-c(broad,morphology,type,relief))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(fov.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()

fov.percent.cover<-fov.points %>%
  group_by(sample)%>%
  mutate_at(vars(starts_with("fov")),funs(./fov.total.points.annotated*100))%>%
  dplyr::select(-c(fov.total.points.annotated))%>%
  glimpse()

# CREATE catami_broad------
broad.points <- habitat %>%
  dplyr::select(-c(fieldofview,morphology,type,relief))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water")) %>%
  dplyr::mutate(broad=paste("broad",broad,sep = ".")) %>%
  dplyr::mutate(count=1) %>%
  dplyr::group_by(sample) %>%
  tidyr::spread(key=broad,value=count,fill=0) %>%
  dplyr::select(-c(image.row,image.col)) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE )) %>%
  ga.clean.names() %>%
  glimpse

broad.percent.cover <- broad.points %>%
  group_by(sample)%>%
  mutate_at(vars(starts_with("broad")),funs(./broad.total.points.annotated*100))%>%
  dplyr::select(-c(broad.total.points.annotated))%>%
  glimpse()


# CREATE catami_morphology------
detailed.points <- habitat%>%
  dplyr::select(-c(fieldofview, relief))%>%
  dplyr::filter(!morphology%in%c("",NA,"Unknown"))%>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water"))%>%
  dplyr::mutate(morphology=paste("detailed",broad,morphology,type,sep = "."))%>%
  dplyr::mutate(morphology=str_replace_all(.$morphology, c(".NA"="","[^[:alnum:] ]"="."," "="","10mm.."="10mm.")))%>%
  dplyr::select(-c(broad,type))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  spread(key=morphology,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(detailed.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse()

detailed.percent.cover<-detailed.points %>%
  group_by(sample)%>%
  mutate_at(vars(starts_with("detailed")),funs(./detailed.total.points.annotated*100))%>%
  dplyr::select(-c(detailed.total.points.annotated))%>%
  glimpse()

# Add kelp onto broad points
broad.points <- detailed.points %>%
  dplyr::select(sample, detailed.macroalgae.largecanopy.forming.eckloniaradiata) %>%
  left_join(broad.points) %>%
  dplyr::rename(broad.kelps = detailed.macroalgae.largecanopy.forming.eckloniaradiata) %>%
  glimpse()

# Create relief----
relief.grid <- habitat%>%
  dplyr::filter(!broad%in%c("Open Water","Unknown"))%>%
  dplyr::filter(!relief%in%c("",NA))%>%
  dplyr::select(-c(broad,morphology,type,fieldofview,image.row,image.col))%>%
  dplyr::mutate(relief.rank=ifelse(relief==".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",0,
                                   ifelse(relief==".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",1,
                                          ifelse(relief==".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",2,
                                                 ifelse(relief==".3. Good relief structure with some overhangs. >45 substrate slope.",3,
                                                        ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",4,
                                                               ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",5,relief)))))))%>%
  dplyr::select(-c(relief))%>%
  dplyr::mutate(relief.rank=as.numeric(relief.rank))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  dplyr::ungroup()%>%
  glimpse()


# Write final habitat data----
setwd(tidy.dir)
dir()

habitat.broad.points <- metadata%>%
  left_join(fov.points, by = "sample")%>%
  left_join(broad.points, by = "sample")%>%
  left_join(relief.grid)

habitat.detailed.points <- metadata%>%
  left_join(fov.points, by = "sample")%>%
  left_join(detailed.points, by = "sample")%>%
  left_join(relief.grid)

habitat.broad.percent <- metadata%>%
  left_join(fov.percent.cover, by = "sample")%>%
  left_join(broad.percent.cover, by = "sample")%>%
  left_join(relief.grid)

habitat.detailed.percent <- metadata%>%
  left_join(fov.percent.cover, by = "sample")%>%
  left_join(detailed.percent.cover, by = "sample")%>%
  left_join(relief.grid)

write.csv(habitat.broad.points,file=paste(study,"random-points_broad.habitat.csv",sep = "_"), row.names=FALSE)
# write.csv(habitat.detailed.points,file=paste(study,"random-points_detailed.habitat.csv",sep = "_"), row.names=FALSE)


# write.csv(habitat.broad.percent,file=paste(study,"random-points_percent-cover_broad.habitat.csv",sep = "_"), row.names=FALSE)
# write.csv(habitat.detailed.percent,file=paste(study,"random-points_percent-cover_detailed.habitat.csv",sep = "_"), row.names=FALSE)

setwd(working.dir)
