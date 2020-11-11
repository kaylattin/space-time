# non-duplicates spatial site

setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)
library(data.table)

d <- read.csv("complete_canada_dataset.csv", header=T)
nd <- read.csv("spatialSiteList_nodups.csv", header = T, check.names = FALSE)

nd_long <- reshape(nd,v.names="RouteNumber",varying = 1:20, timevar="Region",times=names(nd)[1:20],direction='long')

nd_long <- nd_long %>% filter(!is.na(RouteNumber))
nd_long <- select(nd_long, -id)
nd_long$Year <- rep(2018)
nd_long$Transect <- paste(nd_long$RouteNumber, nd_long$Year, sep = ".")

nd_merge <- merge(nd_long, d, by = "Transect", all.x = FALSE)
nd_merge$space.time <- rep(2)

write.csv(nd_merge, "spatialdataset_ND.csv")
