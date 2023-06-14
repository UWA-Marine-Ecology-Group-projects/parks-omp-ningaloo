###
# Project: Parks Ningaloo
# Data:    BRUVS, BOSS Habitat data
# Task:    Merging habitat data
# Author:  Claude Spencer
# Date:    June 2023
##

# Part 1-FSS modeling----

# librarys----
library(tidyr)
library(dplyr)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(reshape2)
library(FSSgam)

rm(list=ls())

name <- "Parks-Ningaloo-synthesis"

# Bring in and format the data----
habi <- readRDS(paste(paste0('data/tidy/', name), 
                      'ga_habitat-bathy-derivatives.rds', sep = "_")) %>%                   # merged data from 'R/02_habitat-model/02_mergedata.R'
  pivot_longer(cols = c("sand", "inverts"),
               names_to = "taxa",
               values_to = "value") %>%
  dplyr::filter(!is.na(roughness)) %>%
  glimpse()

# Set predictor variables---
pred.vars <- c("Z","roughness", "detrended", "aspect",
               "slope")                          

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(habi[ , pred.vars]), 2)

pred.vars <- c("Z","roughness", "detrended", "aspect")                          # Yeet slope  

# # Review of individual predictors for even distribution---
# # Plot of likely transformations - Anna Cresswell loop
# par(mfrow = c(3, 2))
# for (i in pred.vars) {
#   x<-habi[ , i]
#   x = as.numeric(unlist(x))
#   hist((x))#Looks best
#   plot((x), main = paste(i))
#   hist(sqrt(x))
#   plot(sqrt(x))
#   hist(log(x + 1))
#   plot(log(x + 1))
# }
# 
# review and create cols for best transforms

# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(habi$taxa))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=habi[which(habi$taxa==unique.vars[i]),]
  if(length(which(temp.dat$taxa==0))/nrow(temp.dat)<0.9){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use                                                                 # All good  

# Run the full subset model selection----
outdir    <- ("output/gam-habitat/") 
resp.vars <- unique.vars.use
use.dat   <- habi[habi$taxa %in% c(unique.vars.use), ]
out.all   <- list()
var.imp   <- list()


# Loop through the FSS function for each taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- habi[habi$taxa == resp.vars[i],]
  use.dat   <- as.data.frame(use.dat)
    Model1  <- gam(cbind(value, (broad.total.points.annotated - value)) ~ 
                     s(Z, bs = 'cr'),
                   family = binomial("logit"),  data = use.dat)
    
    model.set <- generate.model.set(use.dat = use.dat,
                                    test.fit = Model1,
                                    pred.vars.cont = pred.vars,
                                    # pred.vars.fact=factor.vars,
                                    # linear.vars="Distance",
                                    cyclic.vars = c("aspect"),
                                    k = 5,
                                    cov.cutoff = 0.7
                                    # null.terms = "s(Site, bs='re')"
    )
    out.list <- fit.model.set(model.set,
                              max.models = 600,
                              parallel = T,
                              n.cores = 4)
    names(out.list)
    
    out.list$failed.models # examine the list of failed models
    mod.table <- out.list$mod.data.out  # look at the model selection table
    mod.table <- mod.table[order(mod.table$AICc), ]
    mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
    out.i     <- mod.table[which(mod.table$delta.AICc <= 2), ]
    out.all   <- c(out.all, list(out.i))
    var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))



  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name <- as.character(out.i$modname[m])
    
    png(file = paste(outdir, m, resp.vars[i], "mod_fits.png", sep = ""))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- do.call("rbind", out.all)
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste0(outdir, name, "_all.mod.fits.csv"))
write.csv(all.var.imp,         file = paste0(outdir, name, "_all.var.imp.csv"))

