

###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    Preparing shapefiles for CPLOT Plotter
# author:  Kingsley Griffin
# date:    Aug 2021
##

library(sp)
library(rgdal)
source('R/functions.R')

# Pt Cloates - preferential sites 
ptc_pref <- readOGR("output/planned/ptcloates_pref.shp")
ptcp_df  <- as.data.frame(ptc_pref)

cplot_ptc_pref <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(ptcp_df[, 2:3]),
                             "symbol" = c("Red Star"),
                             "ptcode" = ptcp_df$dropcode,
                             c(0))
head(cplot_ptc_pref)
cplot_ptc_pref <- interaction(cplot_ptc_pref, sep = " ")

write.table(cplot_ptc_pref , "output/planned/cplot/ptc_pref_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Pt Cloates - bruv sites 
ptc_bruv <- readOGR("output/planned/ptcloates_bruv_mbh_spaced.shp")
ptcbr_df  <- as.data.frame(ptc_bruv)
head(ptcbr_df)
cplot_ptc_bruv <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(ptcbr_df[, 1:2]),
                             "symbol" = c("Pink Star"),
                             "ptcode" = ptcbr_df$dropcode,
                             c(0))
head(cplot_ptc_bruv)
cplot_ptc_bruv <- interaction(cplot_ptc_bruv, sep = " ")
head(cplot_ptc_bruv)
write.table(cplot_ptc_bruv , "output/planned/cplot/ptc_bruv_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Pt Cloates - flash boss sites 
ptc_boss <- readOGR("output/planned/ptcloates_flashboss_mbh.shp")
ptcfb_df  <- as.data.frame(ptc_boss)
head(ptcfb_df)
cplot_ptc_boss <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             DDtolatlon(ptcfb_df[, 1:2]),
                             "symbol" = c("Black Star"),
                             "ptcode" = ptcfb_df$dropcode,
                             c(0))
head(cplot_ptc_boss)
cplot_ptc_boss <- interaction(cplot_ptc_boss, sep = " ")
head(cplot_ptc_boss)
write.table(cplot_ptc_boss , "output/planned/cplot/ptc_fboss_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Pt Cloates - baited boss sites 
ptc_bboss <- readOGR("output/planned/ptcloates_baitboss_mbh.shp")
ptcbb_df  <- as.data.frame(ptc_bboss)
head(ptcbb_df)
cplot_ptc_bboss <- data.frame("mark" = c("mark"),
                              "PXYCSLM" = c("PXYCSLM"),
                              DDtolatlon(ptcbb_df[, 1:2]),
                              "symbol" = c("Blue Star"),
                              "ptcode" = ptcbb_df$dropcode,
                              c(0))
head(cplot_ptc_bboss)
cplot_ptc_bboss <- interaction(cplot_ptc_bboss, sep = " ")
head(cplot_ptc_bboss)
write.table(cplot_ptc_bboss , "output/planned/cplot/ptc_bboss_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)


# Yardie - 
shapefile(sites_spdf, "output/planned/yardie_pref", overwrite = TRUE)

# Yardie - preferential
y_pref <- readOGR("output/planned/yardie_pref.shp")
yp_df  <- as.data.frame(y_pref)
head(yp_df)
cplot_y_pref <- data.frame("mark" = c("mark"),
                              "PXYCSLM" = c("PXYCSLM"),
                              DDtolatlon(yp_df[, 2:3]),
                              "symbol" = c("Red Star"),
                              "yode" = yp_df$dropcode,
                              c(0))
head(cplot_y_pref)
cplot_y_pref <- interaction(cplot_y_pref, sep = " ")
head(cplot_y_pref)
write.table(cplot_y_pref , "output/planned/cplot/y_pref_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Yardie -  BRUV
y_bruv <- readOGR("output/planned/yardie_bruv_mbh_spaced.shp")
ybr_df  <- as.data.frame(y_bruv)
head(ybr_df)
cplot_y_bruv <- data.frame("mark" = c("mark"),
                           "PXYCSLM" = c("PXYCSLM"),
                           DDtolatlon(ybr_df[, 1:2]),
                           "symbol" = c("Pink Star"),
                           "yode" = ybr_df$dropcode,
                           c(0))
head(cplot_y_bruv)
cplot_y_bruv <- interaction(cplot_y_bruv, sep = " ")
head(cplot_y_bruv)
write.table(cplot_y_bruv , "output/planned/cplot/y_bruv_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Yardie -  Flash BOSS
y_fboss <- readOGR("output/planned/yardie_flashboss_mbh.shp")
yfb_df  <- as.data.frame(y_fboss)
head(yfb_df)
yfb_df$dropcode <- interaction(c("Y"), yfb_df$method, yfb_df$pointnum, sep="")
cplot_y_fboss <- data.frame("mark" = c("mark"),
                           "PXYCSLM" = c("PXYCSLM"),
                           DDtolatlon(yfb_df[, 1:2]),
                           "symbol" = c("Black Star"),
                           "yode" = yfb_df$dropcode,
                           c(0))
head(cplot_y_fboss)
cplot_y_fboss <- interaction(cplot_y_fboss, sep = " ")
head(cplot_y_fboss)
write.table(cplot_y_fboss , "output/planned/cplot/y_fboss_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Yardie -  Baited BOSS
y_bboss <- readOGR("output/planned/yardie_baitboss_mbh.shp")
ybb_df  <- as.data.frame(y_bboss)
head(ybb_df)
ybb_df$dropcode <- interaction(c("Y"), ybb_df$method, ybb_df$pointnum, sep="")
cplot_y_bboss <- data.frame("mark" = c("mark"),
                            "PXYCSLM" = c("PXYCSLM"),
                            DDtolatlon(ybb_df[, 1:2]),
                            "symbol" = c("Blue Star"),
                            "yode" = ybb_df$dropcode,
                            c(0))
head(cplot_y_bboss)
cplot_y_bboss <- interaction(cplot_y_bboss, sep = " ")
head(cplot_y_bboss)
write.table(cplot_y_bboss , "output/planned/cplot/y_bboss_cplot.txt", sep = "", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)





# couldn't figure out how to get this to work with the quotation marks so I just pasted manually
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
txts <- list.files("output/planned/cplot", "*.txt", full.names = T)
for(filei in txts){
file.copy(filei, paste(gsub(".txt", ".MRK", filei)))
}

