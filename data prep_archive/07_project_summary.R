library(tidyverse)
library(readxl)
library(openxlsx)

#### MAIN DATASET
dat <- read.csv("wholedataset_speciesover40_DEC4.csv")
dat <- read.csv("wholedataset_speciesover40_DEC6_NODUPLICATES.csv")
n_distinct(dat$SpeciesCode)

time <- dat[ dat$space.time == 1, ]
space <- dat[ dat$space.time == 2,]

s <- space %>% group_by(Region) %>%
  summarise(Num_Routes = n_distinct(RouteNumber))

t <- time %>% group_by(Region) %>%
  summarise(Num_Years = n_distinct(Year))


years <- vector("list")
nregions = 20

for(i in 1:nregions) {
  region <- time[time$Region == i ,]
  years[[i]] <- unique(region$Year)
  }

write.xlsx(years, file = "yearspan_regions.xlsx")

# find forest cover range covered by spatial sites
s_min <- space %>% group_by (Region) %>%
  summarise(min_SpatialForest = min(Forest.cover))
s_max <- space %>% group_by (Region) %>%
  summarise(max_SpatialForest = max(Forest.cover))

s_forest <- merge(s_min, s_max, by = "Region")
s_forest$s_Change <- s_forest$max_SpatialForest - s_forest$min_SpatialForest


# repeat for temporal sites

t_min <- time %>% group_by (Region) %>%
  summarise(min_TemporalForest = min(Forest.cover))
t_max <- time %>% group_by (Region) %>%
  summarise(max_TemporalForest = max(Forest.cover))

t_forest <- merge(t_min, t_max, by = "Region")
t_forest$t_Change <- t_forest$max_TemporalForest - t_forest$min_TemporalForest

# merge all together
region <- time %>% group_by(Region, RouteNumber, Change) %>%
  summarise(SpeciesOver40 = n_distinct(SpeciesCode)) # how many species present in that route / year combo

summary <- merge(region, s, by = "Region")

summary <- merge(summary, t, by = "Region")

write.csv(summary,"dataset_summary.csv")
