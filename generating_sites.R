setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)

d <- read.csv("complete_canada_dataset_3.csv", header=T)
forestcover <- read.csv("FORESTCOVER_wide.csv", header=T)
forestcover <- select(forestcover, -c(4:20, 22))

t <- d[which(d$Change >= 20),] # choose sites that experienced a 20% forest decline or greater == 30
t.sites <- distinct(t, RouteNumber, Year, Change, ECOZONE)
t.forest <- merge(t.sites, forestcover, by = "RouteNumber")


s <- d3[which(d$Change < 20),] # the rest are candidate spatial sites == 818
s.sites <- distinct(s, RouteNumber, Year, Change, ECOZONE)
s.sites <- s.sites[which(s.sites$Year == 2018),] ## with data in 2018 == 563
s.sites <- distinct(s.sites, RouteNumber, ECOZONE)
s.forest <- merge(s.sites, forestcover, by = "RouteNumber")
s.forest <- select(s.forest, -X2000)

## Identify of t.sites, how many years of data they have
nsites <- length(unique(t.forest$RouteNumber))

for(i in 1:nsites) {
  years <- count(t.forest$Year[i])
}


## Then, for each temporal site 2000 to 2018 range, select routes in s.sites that represent the same gradient
# in the same ecozone in 2018
