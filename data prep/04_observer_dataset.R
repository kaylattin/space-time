library(tidyverse)

setwd("~/space-time/data prep")

dat <- read.csv("~/space-time/final datasets/whole_dataset_ND_version4.csv")
d <- read.csv("clean_bbs_dataset_mar2021.csv")
bbl <- read.csv("bbl_codes.csv")


d <- merge(d, bbl, by = "English_Common_Name", all.x = TRUE)

dat_obs <- d %>% filter(ObsN %in% dat$ObsN) %>% filter(BBL %in% dat$BBL)

dat_obs$Obs_ID <- as.integer(as.factor(dat_obs$ObsN))
dat_obs$Route_ID <- as.integer(as.factor(dat_obs$RouteNumber))
dat_obs$Eco_ID <- as.integer(as.factor(dat_obs$Ecoregion_L1Code))

dat_obs <- dat_obs[!is.na(dat_obs$Eco_ID),]
n_distinct(dat_obs$ObsN)

write.csv(dat_obs, "observer_dataset_ND_version4.csv")
