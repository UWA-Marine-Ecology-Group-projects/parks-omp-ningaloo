

###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    Preparing shapefiles for CPLOT Plotter
# author:  Kingsley Griffin
# date:    May 2022
##

# library(sp)
library(rgdal)
source('R/functions.R')

# Pt Cloates - bruv sites 
ptc_bruv <- readOGR("output/2205_MBHDesign/planned/ptcloates_bruv_mbh_spaced.shp")

ptcbr_df  <- as.data.frame(ptc_bruv)
head(ptcbr_df)
ptcbr_df <- ptcbr_df[, -c(1, 2)]
colnames(ptcbr_df)[2:5] <- c("dropcode", "selected", "lon", "lat")

# export csv first
head(ptcbr_df)

write.csv(ptcbr_df, "output/2205_MBHDesign/planned/ptcloates_bruv_mbh_spaced.csv")

# sort and export to 
cplot_ptc_bruv <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(ptcbr_df[, 4:5]),
                             "symbol" = c("Pink Star"),
                             "ptcode" = ptcbr_df$dropcode,
                             c(0))
head(cplot_ptc_bruv)
cplot_ptc_bruv <- interaction(cplot_ptc_bruv, sep = " ")
head(cplot_ptc_bruv)
write.table(cplot_ptc_bruv , "output/2205_MBHDesign/planned/ptc_bruv_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Pt Cloates - squid boss sites 
ptc_boss <- readOGR("output/2205_MBHDesign/planned/ptcloates_squidboss_mbh.shp")
ptcfb_df  <- as.data.frame(ptc_boss)
head(ptcfb_df)
cplot_ptc_boss <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(ptcfb_df[, 2:3]),
                             "symbol" = c("Black Star"),
                             "ptcode" = ptcfb_df$dropcod,
                             c(0))
head(cplot_ptc_boss)
cplot_ptc_boss <- interaction(cplot_ptc_boss, sep = " ")
head(cplot_ptc_boss)
write.table(cplot_ptc_boss , "output/2205_MBHDesign/planned/ptc_squidboss_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Pt Cloates - naked boss sites 
ptc_bboss <- readOGR("output/2205_MBHDesign/planned/ptcloates_nakedboss_mbh.shp")
ptcbb_df  <- as.data.frame(ptc_bboss)
head(ptcbb_df)
cplot_ptc_bboss <- data.frame("mark" = c("mark"),
                              "PXYCSLM" = c("PXYCSLM"),
                              DDtolatlon(ptcbb_df[, 2:3]),
                              "symbol" = c("Blue Star"),
                              "ptcode" = ptcbb_df$dropcod,
                              c(0))
head(cplot_ptc_bboss)
cplot_ptc_bboss <- interaction(cplot_ptc_bboss, sep = " ")
head(cplot_ptc_bboss)
write.table(cplot_ptc_bboss , "output/2205_MBHDesign/planned/ptc_nakedboss_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)


# Yardie - 

# Yardie -  BRUV
y_bruv <- readOGR("output/2109_planned/yardie_bruv_mbh_spaced.shp")
y_pref <- readOGR("output/2109_planned/yardie_pref.shp")
  
ybr_df <- as.data.frame(y_bruv)
head(ybr_df)
ypref  <- as.data.frame(y_pref, xy = TRUE)
head(ypref)
ypref  <- ypref[ , -c(1, 11)]
head(ypref)

ybr_df <- rbind(ybr_df, ypref)
head(ybr_df)
ybr_df$dropcode <- interaction("B", seq(1:nrow(ybr_df)), sep = "")

ybr_df <- ybr_df[ , -c(1, 2)]
colnames(ybr_df)[6:7] <- c("lon", "lat")
ybr_df <- ybr_df[ , -3]
head(ybr_df)

write.csv(ybr_df, "output/2205_MBHDesign/planned/yardie_bruv_renamed.csv")

cplot_y_bruv <- data.frame("mark" = c("mark"),
                           "PXYCSLM" = c("PXYCSLM"),
                           DDtolatlon(ybr_df[, 5:6]),
                           "symbol" = c("Pink Star"),
                           "ptcode" = ybr_df$dropcode,
                           c(0))
head(cplot_y_bruv)
cplot_y_bruv <- interaction(cplot_y_bruv, sep = " ")
head(cplot_y_bruv)
write.table(cplot_y_bruv , "output/2205_MBHDesign/planned/y_bruv_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Yardie -  Naked BOSS
y_nboss <- readOGR("output/2109_planned/yardie_flashboss_mbh.shp")
ynb_df  <- as.data.frame(y_nboss)
ynb_df  <- rbind(ynb_df, ypref)
ynb_df$dropcode <- interaction(c("N"), ynb_df$pointnum, sep="")
head(ynb_df)

ynb_df <- ynb_df[, c(1,2,6,7)]
head(ynb_df)
write.csv(ynb_df, "output/2205_MBHDesign/planned/yardie_nakedboss_renamed.csv")

cplot_y_nboss <- data.frame("mark" = c("mark"),
                           "PXYCSLM" = c("PXYCSLM"),
                           DDtolatlon(ynb_df[, 1:2]),
                           "symbol" = c("Black Star"),
                           "ptcode" = ynb_df$dropcode,
                           c(0))
head(cplot_y_nboss)
cplot_y_nboss <- interaction(cplot_y_nboss, sep = " ")
head(cplot_y_nboss)
write.table(cplot_y_nboss , "output/2205_MBHDesign/planned/yardie_nboss_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Yardie -  Squid BOSS
y_bboss <- readOGR("output/2109_planned/yardie_baitboss_mbh.shp")
ysb_df  <- as.data.frame(y_bboss)
head(ysb_df)
ysb_df  <- rbind(ysb_df, ypref)
ysb_df$dropcode <- interaction(c("S"), ysb_df$pointnum, sep="")
head(ysb_df)

ysb_df <- ysb_df[, c(1,2,6,7)]
head(ysb_df)
write.csv(ysb_df, "output/2205_MBHDesign/planned/yardie_squidboss_renamed.csv")

cplot_y_sboss <- data.frame("mark" = c("mark"),
                            "PXYCSLM" = c("PXYCSLM"),
                            DDtolatlon(ysb_df[, 1:2]),
                            "symbol" = c("Blue Star"),
                            "ptcode" = ysb_df$dropcode,
                            c(0))
head(cplot_y_sboss)
cplot_y_sboss <- interaction(cplot_y_sboss, sep = " ")
head(cplot_y_sboss)
write.table(cplot_y_sboss , "output/2205_MBHDesign/planned/y_sboss_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)





# # couldn't figure out how to get this to work with the quotation marks so I just pasted manually
# cplot_header <- c("TMQ CPlot Chart Type 2   ",
#                   "Description: Ningaloo      ",
#                   "Bounds: 21.30.0000S,113.20.0000E,22.55.0000S,114.20.0000E      ",
#                   "Format: NM3      ",
#                   "Rev. Date: 110819/160554     ",
#                   "Scale: 1:00      ",
#                   "Mag. Variation: 0     ",
#                   "Cautions:       ",
#                   "  "  ,
#                     "<EOH>       ")
# 
# cplot_header[3] <- paste0("Bounds: "21.30.0000S,113.20.0000E,22.55.0000S,114.20.0000E"      ")

# copy each file and convert the copy to .MRK file
txts <- list.files("output/2205_MBHDesign/planned/cplot", "*.txt", full.names = T)
for(filei in txts){
file.copy(filei, paste(gsub(".txt", ".MRK", filei)))
}

