
###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    MBH Design Preparation - Pt Cloates
# author:  Kingsley Griffin
# date:    July 2021
##

library(raster)

# read in covariate rasters
preds <- readRDS('output/ptc_covariate_rasts.rds')
plot(preds)
preddf <- as.data.frame(preds, xy = TRUE)
preddf <- preddf[is.na(preddf$slope) != TRUE, ]
preddf[is.na(preddf$mp_zone) == TRUE, ] <- 0
summary(preddf)

### SET BASIC SITE/SURVEY PLAN
## 48 BRUVs - 40 balanced design - 8 legacy/exciting spots/captain's pick
## MAX DEPTHs: BOSS to 300m - BRUV to 150m depth
nbruv <- 40

## Build in design shape (stratification biases)
# split sites among each MP zone
zone_split <- data.frame(zones= c("NPZ", "REC", "Open"), 
                         zcode = c(2, 1, 0),
                         split = c(0.5, 0.1, 0.4))                              # split between NPZ, Rec Use, and Open
zone_split$zbruv <- zone_split$split * nbruv
zone_split
#straw.nums

# Generate categorical bathy via quantile cut points
hist(preds$depth)
bathy_qs   <- c(0, 0.3, 0.7, 1)                                                 # use a wider central zone to generate higher probability zone around main features
bathy_cuts <- quantile(preds$depth, bathy_qs)#c( -Inf,0.02,0.04,0.08,0.16,Inf)
cat_bathy  <- raster::cut(preds$depth, breaks = bathy_cuts, na.rm = TRUE)
plot(cat_bathy)

bathy_split <- data.frame(zones = unique(cat_bathy),
                          split = c(0.2, 0.5, 0.3))                             # bias sampling into central range of bathy
bathy_split$zbruv <- bathy_split$split * nbruv                                  # n samples in each zone
bathy_split

# leave for now but need to expand on this using TPI, Slope bias etc


for( mpz in unique(zone_split$zones)){
  if( mpz == "Open")
    zoneID <- extract( x=catB, y=zones$os, cellnumbers=TRUE)
  zoneID <- extract( x=catB, y=zones$MUZ-zones$NPZ, cellnumbers=TRUE)
  else
    zoneID <- raster::extract( x=catT, y=zones[[zz]], cellnumbers=TRUE)
  propsOfstrata <- table( catT@data@values[zoneID[[1]][,"cell"]])
  propsOfstrata <- propsOfstrata / sum( propsOfstrata)
  #if(length(propsOfstrata) == 4)
  tmp <- TPI.targetProps / propsOfstrata #the desired inclusion probs (unstandardised)
  # else
  #   if(length(TPI.targetProps) == 3)
  #     tmp <- Bathy.targetProps2 / propsOfstrata #the desired inclusion probs (unstandardised)
  # else
  #   if(length(TPI.targetProps) == 2)
  #     tmp <- Bathy.targetProps3 / propsOfstrata #the desired inclusion probs (unstandardised)
  # else 
  #   tmp <- TPI.targetProps / propsOfstrata #the desired inclusion probs (unstandardised)
  for( ii in 1:length( propsOfstrata)){
    inclProbs[zoneID[[1]][,"cell"]][zoneID[[1]][,"value"]==ii] <- tmp[ii]
  }
  inclProbs[zoneID[[1]][,"cell"]][is.na( inclProbs[zoneID[[1]][,"cell"]])] <- 0
  inclProbs[zoneID[[1]][,"cell"]] <- inclProbs[zoneID[[1]][,"cell"]] / sum( inclProbs[zoneID[[1]][,"cell"]])
}
inclProbs@data@values[inclProbs@data@values %in% c(0,1,2,3,4,5,6,7,8)] <- NA  #cheats way to crop
plot( inclProbs)

cellStats(inclProbs, 'sum')



