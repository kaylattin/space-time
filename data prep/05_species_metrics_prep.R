library(tidyverse)
library(vegan)


#### MAIN DATASET
dat <- read.csv("whole_dataset_over40_5p.csv")
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

write.csv(total, "whole_dataset_ta.csv")
write.csv(richness, "whole_dataset_richness.csv")
write.csv(diversity, "whole_dataset_diversity.csv")



### OBSERVER DATASET
obs <- read.csv("observer_dataset_over40.csv")
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

write.csv(total_obs, "observer_dataset_ta.csv")
write.csv(richness_obs, "observer_dataset_richness.csv")
write.csv(diversity_obs, "observer_dataset_diversity.csv")

