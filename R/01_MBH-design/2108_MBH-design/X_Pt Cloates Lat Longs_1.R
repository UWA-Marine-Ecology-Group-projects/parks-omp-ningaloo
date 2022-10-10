setwd("//uniwa.uwa.edu.au/userhome/staff0/00104220/My Documents/R Scripts/Pt Cloates Lat Longs")
dir()

library(dplyr)
library(lubridate)

#get boss lat longs

cloates.boss<-read.csv("MEG_Labsheets_2021 - 2021-08_Pt-Cloates_Flasher-BOSS.csv")%>%
  select(-c(Latitude,Longitude))%>%
  mutate(Sample=as.character(Sample)) 


latlong.boss <- read.csv("Pt Cloates flasher boss MBH_0.csv")%>%
  select(dropcode, x2, y2)%>%
  rename(Sample=dropcode, Latitude = y2, Longitude = x2)

latlong.boss$Sample <- as.character(latlong.boss$Sample)

str(cloates.boss)
str(latlong.boss)

bossfinal <- cloates.boss %>%
  left_join(latlong.boss)

#get bruv lat longs

cloates.bruvs<-read.csv("MEG_Labsheets_2021 - 2021-08_Pt-Cloates_stereo-BRUVs.csv")%>%
  select(-c(Latitude,Longitude))%>%
  mutate(Sample=as.character(Sample))%>%
  mutate(Date = ymd(Date))


latlong.bruvs <- read.csv("Pt Cloates BRUV_recorded-points.csv")%>%
  select(dropcode, x2, y2, EditDate)%>%
  rename(Sample=dropcode, Latitude = y2, Longitude = x2)%>%
  mutate(EditDate = as.character(EditDate))%>%
  mutate(EditDate = mdy_hms(EditDate))%>%
  mutate(EditDate = date(EditDate))%>%
  rename(Date = EditDate)
  

latlong.bruvs$Sample <- as.character(latlong.bruvs$Sample)

str(cloates.bruvs)
str(latlong.bruvs)


bruvfinal <- cloates.bruvs %>%
  left_join(latlong.bruvs, by = c('Sample', 'Date'))



#EXPORT DATA
write.csv(bossfinal, "cloates.boss.w.lat.lon.csv")
write.csv(bruvfinal, "cloates.bruv.w.lat.lon.csv")




