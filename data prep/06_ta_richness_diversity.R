library(tidyverse)

dat <- read.csv("wholedataset_speciesover40_NOV18.csv")
n_distinct(dat$ObsN)

# dataset for total abundance at a transect
total <- dat %>% group_by(space.time, Region, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total$ObsN)

# dataset for species richness at a transect
richness <- dat %>% group_by(space.time, Region, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness$ObsN)

