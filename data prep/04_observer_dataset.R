library(tidyverse)

setwd("~/space-time/data prep")

dat <- read.csv("~/space-time/final datasets/whole_dataset_over40_D.csv")
d <- read.csv("clean_bbs_dataset.csv")
bbl <- read.csv("bbl_codes.csv")

# remove the temporal sites that need to be excluded in order to match the ND dataset = 32 regions
dat <- dat %>% filter(!ref == 14410)
dat <- dat %>% filter(!ref == 53063)
dat <- dat %>% filter(!ref == 69057)


d <- merge(d, bbl, by = "English_Common_Name", all.x = TRUE)

obs <- distinct(dat, ObsN)
obs.list <- obs$ObsN

sp <- distinct(dat, BBL)
sp.list <- sp$BBL

dat_obs <- d %>% filter(ObsN %in% obs.list) %>% filter(BBL %in% sp.list)

dat_obs$Obs_ID <- as.integer(as.factor(dat_obs$ObsN))
dat_obs$Route_ID <- as.integer(as.factor(dat_obs$RouteNumber))
dat_obs$Eco_ID <- as.integer(as.factor(dat_obs$Ecoregion_L1Code))


dat_obs <- dat_obs[!is.na(dat_obs$Eco_ID),]
n_distinct(dat_obs$ObsN)

write.csv(dat_obs, "~/space-time/final datasets/observer_dataset_over40_D.csv")

### no duplicates
setwd("~/space-time/final datasets")

dat <- read.csv("whole_dataset_over40_ND.csv")


d <- merge(d, bbl, by = "English_Common_Name", all.x = TRUE)

obs <- distinct(dat, ObsN)
obs.list <- obs$ObsN

sp <- distinct(dat, BBL)
sp.list <- sp$BBL

dat_obs <- d %>% filter(ObsN %in% obs.list) %>% filter(BBL %in% sp.list)

dat_obs$Obs_ID <- as.integer(as.factor(dat_obs$ObsN))
dat_obs$Route_ID <- as.integer(as.factor(dat_obs$RouteNumber))
dat_obs$Eco_ID <- as.integer(as.factor(dat_obs$Ecoregion_L1Code))


dat_obs <- dat_obs[!is.na(dat_obs$Eco_ID),]


write.csv(dat_obs, "observer_dataset_over40_ND.csv")
