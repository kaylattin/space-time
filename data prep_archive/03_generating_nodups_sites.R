# non-duplicates spatial site

setwd("/Users/kayla/Documents/BBS data")

library(tidyverse)
library(data.table)

d <- read.csv("complete_canada_dataset.csv", header=T)
nd <- read.csv("spatialSiteList_ND_14regions.csv", header = T, check.names = FALSE)

Region <- c(1,3,4,9,10,11,13:20)
TemporalSite <- c(4105,4141,11057,11309,11402,11407,45040,
           45212,56025,68079,68231,68256,76237,76439)
rindex <- data.frame(Region,TemporalSite)

nd_long <- reshape(nd,v.names="RouteNumber",varying = 1:14, timevar="TemporalSite",times=names(nd)[1:14],direction='long')
nd_long <- nd_long %>% filter(!is.na(RouteNumber))

nd_long <- merge(nd_long, rindex, by = "TemporalSite")


nd_long <- select(nd_long, -id)
nd_long$Year <- rep(2018)
nd_long$Transect <- paste(nd_long$RouteNumber, nd_long$Year, sep = ".")

nd_merge <- merge(nd_long, d, by = "Transect", all.x = FALSE)
nd_merge$space.time <- rep(2)

nd_merge2 <- nd_merge %>% distinct(Transect, SpeciesCode, .keep_all = TRUE)

write.csv(nd_merge, "spatialdataset_DEC6_ND.csv")
