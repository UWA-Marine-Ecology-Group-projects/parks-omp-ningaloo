###
# Project: Parks OMP Ningaloo
# Data:    BRUV fish, habitat
# Task:    Plotting fish importance GAM relationships
# author:  Claude
# date:    Dec 2021-Feb 2022
##

rm(list=ls())

library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(GlobalArchive)
library(stringr)
library(ggplot2)
library(gamm4)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)

# set theme
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# Set the study name
name <- "Parks-Ningaloo-synthesis" # for the study

## Set working directory----
#OR Set manually once
combined.maxn <- readRDS("data/tidy/Parks-Ningaloo-synthesis_gam-abundance.rds")%>%
  glimpse()

combined.length <- readRDS("data/tidy/Parks-Ningaloo-synthesis_gam-length.rds")%>%
  glimpse()

dat <- bind_rows(combined.maxn, combined.length)

# Manually make the most parsimonious GAM models for each taxa ----
#### montes MaxN ####

# MODEL Total abundance (depth + mean relief) ----
dat.total <- dat %>% filter(scientific=="total.abundance")

mod = gam(maxn ~ s(detrended,k=3,bs='cr'), family=tw,data=dat.total)

# predict - mean relief ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20)) %>%
  distinct() %>%
  glimpse()

fits <- predict.gam(mod, newdata = testdata, type='response', se.fit=T)

predicts.total.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# mean relief ----
ggmod.total.detrended <- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.total,aes(x=detrended,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.detrended

# MODEL Species richness (depth + habitat.class) ----
dat.species <- dat %>% filter(scientific=="species.richness")

mod = gam(maxn~s(depth,k=3,bs='cr') + habitat.class, family=tw,data=dat.species)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20),
                        habitat.class = c("sand", "inverts")) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - habitat class ----
testdata <- expand.grid(depth=mean(mod$model$depth),
                        habitat.class = c("sand", "inverts")) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.habitat = testdata%>%data.frame(fits)%>%
  group_by(habitat.class)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit)) %>%
  dplyr::mutate(habitat.class = recode(habitat.class, 
                                       "sand" = "Sand", 
                                       "inverts" = "Sessile invertebrates")) %>%
  ungroup()

# PLOTS for Species richness ----
# depth ----
ggmod.species.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.species,aes(x=depth,y=maxn),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.species.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.species.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.species.depth

# habitat class ----
ggmod.species.habitat<- ggplot(data = predicts.species.habitat, aes(x = habitat.class, y = maxn,
                                                                    colour = habitat.class, fill = habitat.class), show.legend = F) +
  ylab("")+
  xlab("Depth")+
  geom_bar(stat = 'identity', show.legend = F) +
  geom_errorbar(aes(ymin = maxn - se.fit, ymax = maxn + se.fit), show.legend = F) +
  scale_fill_manual(labels = c("Sand", "Sessile invertebrates"),values=c("wheat", "plum"))+
  scale_colour_manual(labels = c("Sand", "Sessile invertebrates"),values=c("wheat", "plum"))+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.species.habitat

# MODEL Legals (mean relief) ----
dat.legal <- dat %>% filter(scientific=="greater than legal size")

mod=gam(number~ s(detrended,k=3,bs='cr'), family=tw,data=dat.legal)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Legals ----
# detrended ----
ggmod.legal.detrended <- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.legal,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.legal.detrended,aes(x=detrended,y=maxn),alpha=0.5)+
  geom_line(data=predicts.legal.detrended,aes(x=detrended,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.detrended,aes(x=detrended,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Legal") +
  theme(plot.title = element_text(hjust = 0))
ggmod.legal.detrended

# MODEL Sublegals (depth) ----
dat.sublegal <- dat %>% filter(scientific=="smaller than legal size")

mod=gam(number~s(depth,k=3,bs='cr'), 
        family=tw,data=dat.sublegal)

# predict - depth ----
testdata <- expand.grid(depth=seq(min(dat$depth),max(dat$depth),length.out = 20)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.depth = testdata%>%data.frame(fits)%>%
  group_by(depth)%>% #only change here
  summarise(maxn=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Sublegals ----
# depth ----
# detrended ----
ggmod.sublegal.depth <- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.sublegal,aes(x=depth,y=number),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sublegal.depth,aes(x=depth,y=maxn),alpha=0.5)+
  geom_line(data=predicts.sublegal.depth,aes(x=depth,y=maxn - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.depth,aes(x=depth,y=maxn + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Sublegal") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sublegal.depth

# Combine with patchwork
library(patchwork)

# view plots
plot.grid.gam <- ggmod.total.detrended + plot_spacer() + 
  ggmod.species.depth +  ggmod.species.habitat +  
  ggmod.legal.detrended + plot_spacer() + 
  ggmod.sublegal.depth + plot_spacer() + 
  plot_annotation(tag_levels = 'a') + plot_layout(ncol = 2, nrow = 4)
plot.grid.gam


#Save plots
save_plot(paste0("figures/fish/", name, "_gam-plots.png"), plot.grid.gam,base_height = 9,base_width = 8.5)
