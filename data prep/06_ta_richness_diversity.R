library(tidyverse)


#### MAIN DATASET
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


write.csv(total, "wholedataset_totalabundance.csv")
write.csv(richness, "wholedataset_richness.csv")




### OBSERVER DATASET
obs <- read.csv("observerdataset_NOV12.csv")
n_distinct(obs$ObsN)

total_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID ) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total_obs$ObsN)

# dataset for species richness at a transect
richness_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID ) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness_obs$ObsN)


write.csv(total_obs, "observerdataset_totalabundance.csv")
write.csv(richness_obs, "wholedataset_richness.csv")

