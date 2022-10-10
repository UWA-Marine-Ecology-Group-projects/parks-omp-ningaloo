#get lat longs for yardie and join with metadata

library(dplyr)
library(stringr)

setwd('//uniwa.uwa.edu.au/userhome/staff0/00104220/My Documents/GitHub/parks-omp-ningaloo/data/Staging')
dir()

#get flasher boss lat longs

yardie.flash.boss<-read.csv("MEG_Labsheets_2021 - 2021-08_Yardie-Creek_Flasher-BOSS (1).csv")%>%
  select(-c(Latitude,Longitude))%>%
  mutate(Sample=as.character(Sample))


latlong.flash.boss <- read.csv("Yardie flash BOSS_0.csv")%>%
  select(dropcode, x2, y2)%>%
  rename(Sample=dropcode, Latitude = y2, Longitude = x2)%>%
  mutate(Sample=str_replace_all(.$Sample,c("N"=""))) 

latlong.flash.boss$Sample <- as.character(latlong.flash.boss$Sample)

str(yardie.flash.boss)
str(latlong.flash.boss)


flashbossfinal <- yardie.flash.boss %>%
  left_join(latlong.flash.boss)

#get baited boss lat longs

yardie.bait.boss<-read.csv("MEG_Labsheets_2021 - 2021-08_Yardie-Creek_Baited-BOSS (1).csv")%>%
  select(-c(Latitude,Longitude))%>%
  mutate(Sample=as.character(Sample))


latlong.bait.boss <- read.csv("Yardie Bait BOSS_0.csv")%>%
  select(dropcode, x2, y2)%>%
  rename(Sample=dropcode, Latitude = y2, Longitude = x2)%>%
  mutate(Sample=str_replace_all(.$Sample,c("N"=""))) 

latlong.bait.boss$Sample <- as.character(latlong.bait.boss$Sample)

str(yardie.bait.boss)
str(latlong.bait.boss)


baitbossfinal <- yardie.bait.boss %>%
  left_join(latlong.bait.boss)

test <- baitbossfinal %>%
  group_by(Sample)%>%
  summarise(n=n())


#get bruv lat longs

yardie.bruv<-read.csv("MEG_Labsheets_2021 - 2021-08_Yardie-Creek_stereo-BRUVs (1).csv")%>%
  select(-c(Latitude,Longitude))%>%
  mutate(Sample=as.character(Sample))


latlong.bruv <- read.csv("Yardie BRUV MBH_0.csv")%>%
  select(dropcode, x2, y2)%>%
  rename(Sample=dropcode, Latitude = y2, Longitude = x2)%>%
  mutate(Sample=str_replace_all(.$Sample,c("N"=""))) 

latlong.bruv$Sample <- as.character(latlong.bruv$Sample)

str(yardie.bruv)
str(latlong.bruv)


bruvfinal <- yardie.bruv %>%
  left_join(latlong.bruv)

#EXPORT DATA
write.csv(flashbossfinal, "yardie.flash.boss.w.lat.lon.csv")
write.csv(baitbossfinal, "yardie.bait.boss.w.lat.lon.csv")
write.csv(bruvfinal, "yardie.bruv.w.lat.lon.csv")
