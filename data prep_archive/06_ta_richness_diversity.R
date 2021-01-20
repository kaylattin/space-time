library(tidyverse)
library(vegan)


#### MAIN DATASET
dat <- read.csv("wholedataset_speciesover40_NOV18.csv")
dat <- read.csv("wholedataset_speciesover40_DEC6_NODUPLICATES.csv")
n_distinct(dat$ObsN)

# dataset for total abundance at a transect
total <- dat %>% group_by(space.time, Region, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total$ObsN)

# dataset for species richness at a transect
richness <- dat %>% group_by(space.time, Region, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness$ObsN)

diversity <- dat %>% group_by(space.time, Region, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Hprime = diversity(Count, index = "shannon"))
n_distinct(diversity$ObsN)

write.csv(total, "wholedataset_totalabundance_ND.csv")
write.csv(richness, "wholedataset_richness_ND.csv")
write.csv(diversity, "wholedataset_diversity_ND.csv")



### OBSERVER DATASET
obs <- read.csv("observerdataset_NOV23.csv")
obs <- read.csv("observerdataset_DEC6_NODUPLICATES.csv")
n_distinct(obs$ObsN)

total_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total_obs$ObsN)

# dataset for species richness at a transect
richness_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness_obs$ObsN)

diversity_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID) %>%
  summarise(Hprime = diversity(Count, index = "shannon"))
n_distinct(diversity$ObsN)

write.csv(total_obs, "observerdataset_totalabundance_ND.csv")
write.csv(richness_obs, "observerdataset_richness_ND.csv")
write.csv(diversity_obs, "observerdataset_diversity_ND.csv")

