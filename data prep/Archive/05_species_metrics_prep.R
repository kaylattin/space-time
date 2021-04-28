library(tidyverse)
library(vegan)
library(openxlsx)
library(readxl)
setwd("~/space-time/data prep")
spatial <- read.csv("~/space-time/data prep/SR1_total/spatial_dataset_mar2021_version4.csv")
temporal <- read.csv("~/space-time/data prep/SR1_total/temporal_dataset_mar2021_version4.csv")
forestcodes <- read.csv("forestcodes_SW.csv", header = T)
forestcodes <- forestcodes %>% dplyr::select(English_Common_Name, status) %>% distinct(English_Common_Name, status)
bbl <- read.csv("bbl_codes.csv") # had to make manual changes to YRWA, DEJU in Excel as well as add Sooty Grouse, Ruffed Grouse, Northern Bobwhite 
# since BBL doesn't provide codes for gallinaceous birds

new <- read.csv("~/space-time/final datasets/SR2_mean_forest/richness_dataset_100m_v3.csv")

reg <- unique(new$ref)
# some prep - removing spatial & temporal sites that have <10 spatial matches or bbs years
# leaves me with 40 regions!

d <- rbind(spatial, temporal)
d <- d %>% filter(ref %in% reg)

d <- merge(d, bbl, by = "English_Common_Name")
d <- merge(d, forestcodes, by = "English_Common_Name")

d <- d %>% filter(!status == "W")
d <- d %>% filter(!status == "S")
d <- d %>% filter(!status == "N")
# d <- d %>% filter(!Count == 0)

# remove the temporal sites that need to be excluded in order to match the ND dataset = 32 regions
# d <- d %>% filter(!ref == 14410)
# d <- d %>% filter(!ref == 53063)
# d <- d %>% filter(!ref == 69057)

n_distinct(d$BBL)
n_distinct(d$ref)

# dataset for total abundance at a transect
total <- d %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total$ObsN)

# dataset for species richness at a transect
richness <- d %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness$ObsN)

diversity <- d %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Hprime = diversity(Count, index = "shannon"))
n_distinct(diversity$ObsN)

write.csv(total, "whole_dataset_ta_mar2021_version4.csv")
write.csv(richness, "~/space-time/final datasets/SR1_total/total_richness_dataset_FINAL.csv")
write.csv(diversity, "whole_dataset_diversity_mar2021_version4.csv")




dat <- read.csv("clean_bbs_dataset_mar2021.csv")
dat <- merge(dat, bbl, by = "English_Common_Name", all.x = TRUE)

obs <- dat %>% filter(ObsN %in% d$ObsN)

obs$Obs_ID <- as.integer(as.factor(obs$ObsN))
obs$Route_ID <- as.integer(as.factor(obs$RouteNumber))
obs$Eco_ID <- as.integer(as.factor(obs$Ecoregion_L1Code))

obs <- obs[!is.na(obs$Eco_ID),]
n_distinct(obs$ObsN)

### OBSERVER DATASET
total_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total_obs$ObsN)

# dataset for species richness at a transect
richness_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness_obs$ObsN)

diversity_obs <- obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID) %>%
  summarise(Hprime = diversity(Count, index = "shannon"))
n_distinct(diversity_obs$ObsN)

write.csv(total_obs, "observer_dataset_ta_version4.csv")
write.csv(richness_obs, "observer_dataset_richness_version4.csv")
write.csv(diversity_obs, "observer_dataset_diversity_version.csv")



## no duplicates
setwd("~/space-time/final datasets")

#### MAIN DATASET
dat <- read.csv("whole_dataset_over40_ND.csv")
n_distinct(dat$ObsN)

# dataset for total abundance at a transect
total <- dat %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(TotalAbundance = sum(Count))
n_distinct(total$ObsN)

# dataset for species richness at a transect
richness <- dat %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo
n_distinct(richness$ObsN)

diversity <- dat %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover) %>%
  summarise(Hprime = diversity(Count, index = "shannon"))
n_distinct(diversity$ObsN)

write.csv(total, "whole_dataset_ta_ND.csv")
write.csv(richness, "whole_dataset_richness_ND.csv")
write.csv(diversity, "whole_dataset_diversity_ND.csv")



### OBSERVER DATASET
obs <- read.csv("observer_dataset_over40_ND.csv")
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

write.csv(total_obs, "observer_dataset_ta_ND.csv")
write.csv(richness_obs, "observer_dataset_richness_ND.csv")
write.csv(diversity_obs, "observer_dataset_diversity_ND.csv")


