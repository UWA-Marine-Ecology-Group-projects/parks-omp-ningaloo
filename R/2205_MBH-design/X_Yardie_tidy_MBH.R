library(sp)
library(dplyr)
library(rgdal)

working.dir <-getwd()
setwd(working.dir)


bruv <- as.data.frame(readOGR("output/planned/yardie_bruv_mbh_spaced.shp"))%>%
  mutate(dropcode = paste0("B",seq(1,16)))%>%
  select(lon,lat,site,method,dropcode)

squid <- as.data.frame(readOGR("output/planned/yardie_baitboss_mbh.shp"))%>%
  mutate(dropcode = paste0("S",seq(100,123)), method = "SquidBOSS")%>%
  select(lon,lat,site,method,dropcode)

naked <- as.data.frame(readOGR("output/planned/yardie_flashboss_mbh.shp"))%>%
  mutate(dropcode = paste0("N",seq(200,223)), method = "NakedBOSS")%>%
  select(lon,lat,site,method,dropcode)

write.csv(bruv, file = "output/2205_MBHDesign/planned/yardie_bruv_renamed.csv", row.names = F)
write.csv(squid, file = "output/2205_MBHDesign/planned/yardie_squidboss_renamed.csv", row.names = F)
write.csv(naked, file = "output/2205_MBHDesign/planned/yardie_nakedboss_renamed.csv", row.names = F)
