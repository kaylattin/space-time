setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)

spatial <- read.csv("spatialdataset_NOV11.csv")
spatial_nd <- read.csv("spatialdataset_ND.csv")
temporal <- read.csv("temporaldataset_NOV11.csv")

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
matched_species <- intersect(s.species, t.species) # 134 species in both datasets; 93 if forest bird only

spatial_new <- spatial[spatial$SpeciesCode %in% matched_species$SpeciesCode ,]
spatial_new_nd <- spatial_nd[spatial_nd$SpeciesCode %in% matched_species$SpeciesCode , ]
temporal_new <- temporal[temporal$SpeciesCode %in% matched_species$SpeciesCode ,]

write.csv(spatial_new, "spatialdataset_final.csv")
write.csv(spatial_new_nd, "spatialdataset_ND_final.csv")
write.csv(temporal_new, "temporaldataset_final.csv")


data <- rbind(spatial_new, temporal_new)
write.csv(data, "wholedataset_D.csv")

data_nd <- rbind(spatial_new_nd, temporal_new)
write.csv(data_nd, "wholedataset_ND.csv")




#### WORK IN PROGRESS - tread carefulyl ------------
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




## trying to get a list of species for every spatial site
spatial$routeregion <- paste(spatial$RouteNumber.x, spatial$Region, sep="x")
speciesBySpatial <- distinct(spatial, Region, routeregion, SpeciesCode)
speciesBySpatial$Region <- as.factor(speciesBySpatial$Region)
speciesBySpatial <- speciesBySpatial[order(speciesBySpatial$Region),]

new <- split(speciesBySpatial, f = speciesBySpatial$Region)

r1 <- data.frame(new[[1]])
r1$index <- as.numeric(as.factor(r1$routeregion))
r1_split <- split(r1, f = r1$index)

r1_species <- intersect(r1_split[1], r1_split[2])
