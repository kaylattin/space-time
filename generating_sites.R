setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)
library(data.table)
d <- read.csv("complete_canada_dataset.csv", header=T)
forestcover <- read.csv("FORESTCOVER_wide.csv", header=T)
forestcover <- select(forestcover, -c(4:20, 22))

t <- d[which(d$Change >= 20),] # choose sites that experienced a 20% forest decline or greater == 30
t.sites <- distinct(t, RouteNumber, Year, Change, ECOZONE)
t.forest <- merge(t.sites, forestcover, by = "RouteNumber")


s <- d[which(d$Change < 20),] # the rest are candidate spatial sites == 818
s.sites <- distinct(s, RouteNumber, Year, Change, ECOZONE)
s.sites <- s.sites[which(s.sites$Year == 2018),] ## with data in 2018 == 563
s.sites <- distinct(s.sites, RouteNumber, ECOZONE)
s.forest <- merge(s.sites, forestcover, by = "RouteNumber")
s.forest <- select(s.forest, -X2000)

## Identify of t.sites, how many years of data they have
t.sitesByYear <- t.forest %>% group_by(RouteNumber) %>%
  summarise(Count = n_distinct(Year))

# Select only for sites with at least 7 years of data
t.sitesByYear <- t.sitesByYear[which(t.sitesByYear$Count >= 7),]
t.list <- t.sitesByYear$RouteNumber

t.sites <- t.forest[t.forest$RouteNumber %in% t.list ,]
t.sites <- distinct(t.sites, RouteNumber, X2000, X2018, ECOZONE)

## Then, for each temporal site 2000 to 2018 range, after rounding to nearest 1%, and giving an "sd" of 2%,
# select routes in s.sites that represent the same forest % gradient
# in the same ecozone in 2018
# need to continuously refer back to temporal object to adjust numbers for this

# temporal = route 4105
s1 <- s.forest[which(s.forest$X2018 <= 72 & s.forest$X2018 >= 3 & s.forest$ECOZONE == 14),]
s1$Region <- rep(1)
l1 <- s1$RouteNumber

# temporal = route 4116
s2 <- s.forest[which(s.forest$X2018 <= 58 & s.forest$X2018 >= 34 & s.forest$ECOZONE == 9),]
s2$Region <- rep(2)
l2 <- s2$RouteNumber

# temporal = route 4141
s3 <- s.forest[which(s.forest$X2018 <= 64 & s.forest$X2018 >= 24 & s.forest$ECOZONE == 9),]
s3$Region <- rep(3)
l3 <- s3$RouteNumber

# temporal = route 11057
s4 <- s.forest[which(s.forest$X2018 <= 70 & s.forest$X2018 >= 23 & s.forest$ECOZONE == 14),]
s4$Region <- rep(4)
l4 <- s4$RouteNumber

# temporal = route 11068
s5 <- s.forest[which(s.forest$X2018 <= 69 & s.forest$X2018 >= 41 & s.forest$ECOZONE == 14),]
s5$Region <- rep(5)
l5 <- s5$RouteNumber

# temporal = route 11202
s6 <- s.forest[which(s.forest$X2018 <= 88 & s.forest$X2018 >= 63 & s.forest$ECOZONE == 13),]
s6$Region <- rep(6)
l6 <- s6$RouteNumber

# temporal = route 11234
s7 <- s.forest[which(s.forest$X2018 <= 64 & s.forest$X2018 >= 32 & s.forest$ECOZONE == 14),]
s7$Region <- rep(7)
l7 <- s7$RouteNumber

# temporal = route 11236
s8 <- s.forest[which(s.forest$X2018 <= 38 & s.forest$X2018 >= 14 & s.forest$ECOZONE == 14),]
s8$Region <- rep(8)
l8 <- s8$RouteNumber

# temporal = route 11256
s9 <- s.forest[which(s.forest$X2018 <= 75 & s.forest$X2018 >= 43 & s.forest$ECOZONE == 14),]
s9$Region <- rep(9)
l9 <- s9$RouteNumber

# temporal = route 11309
s10 <- s.forest[which(s.forest$X2018 <= 72 & s.forest$X2018 >= 10 & s.forest$ECOZONE == 14),]
s10$Region <- rep(10)
l10 <- s10$RouteNumber

# temporal = route 11402
s11 <- s.forest[which(s.forest$X2018 <= 70 & s.forest$X2018 >= 40 & s.forest$ECOZONE == 13),]
s11$Region <- rep(11)
l11 <- s11$RouteNumber

# temporal = route 11407
s12 <- s.forest[which(s.forest$X2018 <= 38 & s.forest$X2018 >= 14 & s.forest$ECOZONE == 14),]
s12$Region <- rep(12)
l12 <- s12$RouteNumber

# temporal = route 11614
s13 <- s.forest[which(s.forest$X2018 <= 76 & s.forest$X2018 >= 34 & s.forest$ECOZONE == 13),]
s13$Region <- rep(13)
l13 <- s13$RouteNumber

# temporal = route 45040
s14 <- s.forest[which(s.forest$X2018 <= 76 & s.forest$X2018 >= 50 & s.forest$ECOZONE == 6),]
s14$Region <- rep(14)
l14 <- s14$RouteNumber

# temporal = route 45212
s15 <- s.forest[which(s.forest$X2018 <= 66 & s.forest$X2018 >= 33 & s.forest$ECOZONE == 6),]
s15$Region <- rep(15)
l15 <- s14$RouteNumber

# temporal = route 56025
s16 <- s.forest[which(s.forest$X2018 <= 79 & s.forest$X2018 >= 54 & s.forest$ECOZONE == 7),]
s16$Region <- rep(16)
l16 <- s15$RouteNumber

# temporal = route 68076
s17 <- s.forest[which(s.forest$X2018 <= 70 & s.forest$X2018 >= 43 & s.forest$ECOZONE == 6),]
s17$Region <- rep(17)
l17 <- s5$RouteNumber

# temporal = route 68079
s18 <- s.forest[which(s.forest$X2018 <= 56 & s.forest$X2018 >= 25 & s.forest$ECOZONE == 6),]
s18$Region <- rep(18)
l18 <- s18$RouteNumber

# temporal = route 68231
s19 <- s.forest[which(s.forest$X2018 <= 78 & s.forest$X2018 >= 54 & s.forest$ECOZONE == 6),]
s19$Region <- rep(19)
l19 <- s19$RouteNumber

# temporal = route 68256
s20 <- s.forest[which(s.forest$X2018 <= 66 & s.forest$X2018 >= 40 & s.forest$ECOZONE == 6),]
s20$Region <- rep(20)
l20 <- s20$RouteNumber

# temporal = route 76237
s21 <- s.forest[which(s.forest$X2018 <= 85 & s.forest$X2018 >= 60 & s.forest$ECOZONE == 6),]
s21$Region <- rep(21)
l21 <- s21$RouteNumber

# temporal = route 76281
s22 <- s.forest[which(s.forest$X2018 <= 73 & s.forest$X2018 >= 44 & s.forest$ECOZONE == 6),]
s22$Region <- rep(22)
l22 <- s22$RouteNumber

# temporal = route 76439
s23 <- s.forest[which(s.forest$X2018 <= 81 & s.forest$X2018 >= 56 & s.forest$ECOZONE == 6),]
s23$Region <- rep(23)
l23 <- s23$RouteNumber


### dataframe of spatial lists for each temporal site
s.data <- data.table(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23)
write.csv(s.data,"spatialSiteList.csv") # needs some post-processing in excel to remove recycling in columns

### get the list of spatial sites with duplicates
spatial <- rbind(s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,
                    s11,s12,s13,s14,s15,s16,s17,s18,s19,
                    s20,s21)

spatial$Year <- rep(2018)
spatial$Transect <- paste(spatial$RouteNumber, spatial$Year, sep=".")
spatial_merge <- merge(spatial, d, by = "Transect", all.x = FALSE)
spatial_merge$space.time <- rep(2)

write.csv(spatial_merge,"spatialdataset_NOV9_23.csv")

### temporal dataset
temporal <- d[d$RouteNumber %in% t.list ,]
temporal$Transect <- paste(temporal$RouteNumber, temporal$Year, sep=".")
temporal$space.time <- rep(1)
region_index <- data.table(
  RouteNumber = t.sites$RouteNumber,
  Region = seq(1:23)
)
temporal <- merge(temporal, region_index, by = "RouteNumber")
write.csv(temporal, "temporaldataset_NOV9_23.csv")
