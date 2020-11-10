setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)

spatial <- read.csv("spatialdataset_NOV9_23_D.csv")
temporal <- read.csv("temporaldataset_NOV9_23.csv")

# find species that are not present at ANY of the spatial sites
s.species <- spatial %>% group_by(SpeciesCode) %>%
  summarise_at(vars(Count),list(Count = sum))
s.species <- filter(s.species, !Count == 0)


# find species that are not present at ANY of the temporal sites
t.species <- temporal %>% group_by(SpeciesCode) %>%
  summarise_at(vars(Count), list(Count = sum))
t.species <- filter(t.species, !Count == 0)

# nspecies in spatial = 151, nspecies in temporal = 143
# find only matched species that appear in both datassets
t.species <- select(t.species, -Count)
s.species <- select(s.species,  -Count)
matched_species <- intersect(s.species, t.species) # 134 species in both datasets

spatial_new <- spatial[spatial$SpeciesCode %in% matched_species$SpeciesCode ,]
temporal_new <- temporal[temporal$SpeciesCode %in% matched_species$SpeciesCode ,]

# get species lists by region for each spatial and temporal site
t <- vector("list")
s <- vector("list")
species <- vector("list")

s.species <- spatial %>% group_by(SpeciesCode, Region) %>%
  summarise_at(vars(Count),list(Count = sum))
t.species <- temporal %>% group_by(SpeciesCode, Region) %>%
  summarise_at(vars(Count), list(Count = sum))
t.species <- select(t.species, -Count)
s.species <- select(s.species,  -Count)

for(i in 1:23) {
  t[[i]] <- filter(t.species, Region == i)
  s[[i]] <- filter(s.species, Region == i)
  
  species[[i]] <- intersect(s[[i]], t[[i]])
}

# species object is a vector of lists[[i]], where i runs from 1 to 23 for each region
# each region list contains the species matched in both the spatial and temporal dataset FOR THAT REGION

for(i in 1:23) {
  write.csv(species[[i]], paste('matched_species',i,'csv', sep='.'))
}
