setwd("//uniwa.uwa.edu.au/userhome/staff0/00104220/My Documents/R Scripts/Pt Cloates Lat Longs Round 2")
dir()

library(dplyr)
library(lubridate)
library(stringr)

#get boss lat longs

cloates.boss<-read.csv("MEG_Labsheets_2021 - 2021-08_Pt-Cloates_Flasher-BOSS (1).csv")%>%
  select(-c(Latitude,Longitude))%>%
  mutate(Sample=as.character(Sample)) 


latlong.boss <- read.csv("Pt Cloates flasher boss MBH_0.csv")%>%
  select(dropcode, x2, y2)%>%
  rename(Sample=dropcode, Latitude = y2, Longitude = x2)%>%
  mutate(Sample=str_replace_all(.$Sample,c("N"=""," CORRECT" = "")))

latlong.boss$Sample <- as.character(latlong.boss$Sample)

str(cloates.boss)
str(latlong.boss)

bossfinal <- cloates.boss %>%
  left_join(latlong.boss)

test <- bossfinal %>%
  group_by(Sample)%>%
  summarise(n=n())
#get bruv lat longs

cloates.bruvs<-read.csv("MEG_Labsheets_2021 - 2021-08_Pt-Cloates_stereo-BRUVs.csv")%>%
  select(-c(Latitude,Longitude))%>%
  mutate(Sample=as.character(Sample))


latlong.bruvs <- read.csv("Pt Cloates BRUV MBH spaced_0.csv")%>%
  select(dropcode, x2, y2)%>%
  rename(Sample=dropcode, Latitude = y2, Longitude = x2)%>%
  mutate(Sample=str_replace_all(.$Sample,c("N"="")))
  

latlong.bruvs$Sample <- as.character(latlong.bruvs$Sample)

str(cloates.bruvs)
str(latlong.bruvs)


bruvfinal <- cloates.bruvs %>%
  left_join(latlong.bruvs, by = c('Sample'))


#SQUID BOSS

dir()

cloates.squid.boss<-read.csv("MEG_Labsheets_2021 - 2021-08_Pt-Cloates_Squid-BOSS.csv")%>%
  select(-c(Latitude,Longitude))%>%
  mutate(Sample=as.character(Sample)) 


latlong.squid.boss <- read.csv("Pt Cloates Baited BOSS_0.csv")%>%
  select(dropcode, x2, y2)%>%
  rename(Sample=dropcode, Latitude = y2, Longitude = x2)

latlong.squid.boss$Sample <- as.character(latlong.squid.boss$Sample)

str(cloates.squid.boss)
str(latlong.squid.boss)

squid.bossfinal <- cloates.squid.boss %>%
  left_join(latlong.squid.boss)



#EXPORT DATA
write.csv(bossfinal, "cloates.boss.w.lat.lon.csv")
write.csv(bruvfinal, "cloates.bruv.w.lat.lon.csv")
write.csv(squid.bossfinal, "cloates.squid.boss.w.lat.lon.csv")



