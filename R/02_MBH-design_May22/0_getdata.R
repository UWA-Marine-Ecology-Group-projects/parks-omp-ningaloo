
###
# Project: Parks - Ningaloo Post-Survey
# Data:    BRUVS, BOSS
# Task:    Getting data from labsheets
# author:  Kingsley Griffin
# date:    July 2021
##

library(googlesheets4)

# get sampling data
tsboss <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592",
                                  sheet = "2021-05_NingalooTransect_BOSS"))
pcbruv <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592",
                                  sheet = "2021-05_PtCloates_stereo-BRUVS"))
pcboss <- as.data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=814068592",
                                   sheet = "2021-05_PtCloates_BOSS"))

saveRDS(tsboss, 'data/2105_ningaloo_tsboss.rds')
saveRDS(pcbruv, 'data/2105_ptcloates_bruv.rds')
saveRDS(pcboss, 'data/2105_ptcloates_boss.rds')
