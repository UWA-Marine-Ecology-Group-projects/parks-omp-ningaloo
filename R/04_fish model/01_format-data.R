###
# Project: ** Add here **
# Data:    BRUV and BOSS fish MaxN and length data
# Task:    Format data for use in FSSgam model selection
# Author:  Claude Spencer & Brooke Gibbons from beckyfisher - FSSgam
# Date:    ** Add here **
##

rm(list=ls())

# libraries----
library(tidyr)
library(dplyr)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
# library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)
library(purrr)
library(readr)
library(corrr)

campaignid <- "2021-05_Abrolhos_BOSS-BRUV"                                      # CampaignID name for the synthesis
name       <- "Abrolhos"

# Load and join datasets
# MaxN
maxn <- list.files(path = "data/tidy/",
                           pattern = "*.complete.maxn.csv",
                           full.names = T) %>%
  lapply(., function(x){read.csv(x)}) %>%
  bind_rows() %>%
  mutate(method = if_else(str_detect(campaignid, "BOSS"), "BOSS", "BRUV")) %>%
  glimpse()

# Length
length <- list.files(path = "data/tidy/",
                   pattern = "*.complete.length.csv",
                   full.names = T) %>%
  lapply(., function(x){read.csv(x)}) %>%
  bind_rows() %>%
  mutate(method = if_else(str_detect(campaignid, "BOSS"), "BOSS", "BRUV"),
         scientific = paste(family, genus, species, sep = " ")) %>%
  glimpse()

# Habitat
allhab <- readRDS(paste(paste0('data/tidy/', name), 
                        'habitat-bathy-derivatives.rds', sep = "_")) %>%        # From 02_habitat model/03_mergedata.R
  ga.clean.names() %>%
  transform(kelps = kelps / broad.total.points.annotated,
            macroalgae = macroalgae / broad.total.points.annotated,
            sand = sand / broad.total.points.annotated,
            rock = rock / broad.total.points.annotated,
            inverts = inverts / broad.total.points.annotated) %>%
  dplyr::select(-c(latitude, longitude, latitude.1, longitude.1)) %>%
  glimpse()

metadata <- maxn %>%
  distinct(sample, method,latitude, longitude, date, time, location, status, site, 
           depth, observer, successful.count, successful.length)

# look at top species ----
maxn.sum <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  top_n(10)%>%
  ungroup()

## Total frequency of occurrence
ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
  geom_bar(stat="identity",position = position_dodge()) +
  coord_flip() +
  xlab("Species") +
  ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
  theme(axis.text.y = element_text(face = "italic"))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific, sample, method) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific, maxn, fill = 0) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(total.abundance = rowSums(.[, 3:(ncol(.))], na.rm = TRUE )) %>% # Add in Totals
  dplyr::mutate(species.richness = rowSums(.[, 3:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample, total.abundance, species.richness, method) %>%
  tidyr::gather(., "scientific", "maxn", 2:3) %>%
  dplyr::glimpse()

dat.maxn <- bind_rows(ta.sr) %>%
  left_join(allhab) %>%
  left_join(metadata) %>%
  distinct()

unique(dat.maxn$scientific)

# Set predictor variables---
names(dat.maxn)

pred.vars = c("depth", 
              "macroalgae", 
              "sand", 
              "inverts", 
              "mean.relief",
              "tpi",
              "roughness",
              "detrended") 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
correlate(dat.maxn[,pred.vars], use = "complete.obs") %>%  
  gather(-term, key = "colname", value = "cor") %>% 
  dplyr::filter(abs(cor) > 0.8)                                         

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow = c(3, 2))
for (i in pred.vars) {
  x <- dat.maxn[ , i]
  x = as.numeric(unlist(x)) 
  hist((x)) #Looks best
  plot((x), main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x + 1))
  plot(log(x + 1))
}

# Write data to load in to next script
saveRDS(dat.maxn, paste(paste0('data/tidy/', name), 
                        'gam-data-maxn.rds', sep = "_"))

#lengths
# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

unique(master$fishing.type)

# Filter by fished species, manually adding in 'spp' fished species and removing species not counted as targeted
fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in% c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp","Lethrinidae Gymnocranius spp",
                                                       "Lethrinidae Lethrinus spp","Lethrinidae Unknown spp","Platycephalidae Platycephalus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae"))%>%    # Remove non-targeted families   
  dplyr::filter(!species%in%c("albimarginatus","longimanus")) %>%               # Remove non-targeted species
  dplyr::mutate(minlegal.wa = as.double(minlegal.wa)) %>%
  glimpse()

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) 

legal <- fished.species %>%
  tidyr::replace_na(list(minlegal.wa=0)) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()

combined.length <- bind_rows(legal, sublegal) 

unique(combined.length$scientific)

dat.length <- combined.length %>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(sample,scientific,number,method) %>%
  tidyr::complete(nesting(sample,method), scientific) %>%
  replace_na(list(number = 0)) %>% # we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,allhab) %>%
  dplyr::filter(successful.length%in%c("Y", "Yes", "yes")) %>%
  dplyr::mutate(scientific = as.character(scientific)) %>%
  dplyr::glimpse()

# Set predictor variables---
pred.vars = c("depth", 
              "macroalgae", 
              "sand", 
              "inverts", 
              "mean.relief",
              "tpi",
              "roughness",
              "detrended") 

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
correlate(dat.maxn[,pred.vars], use = "complete.obs") %>%  
  gather(-term, key = "colname", value = "cor") %>% 
  dplyr::filter(abs(cor) > 0.8) 

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat.length[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

#write data to load in to next script
saveRDS(dat.maxn, paste(paste0('data/tidy/', name), 
                        'gam-data-length.rds', sep = "_"))
