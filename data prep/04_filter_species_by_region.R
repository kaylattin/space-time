setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)

spatial <- read.csv("spatialdataset_NOV12_D.csv")
spatial_nd <- read.csv("spatialdataset_NOV12_ND.csv")
temporal <- read.csv("temporaldataset_NOV12.csv")

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
matched_species <- intersect(s.species, t.species) # 134 species in both datasets; 92 if forest bird only

spatial_new <- spatial[spatial$SpeciesCode %in% matched_species$SpeciesCode ,]
spatial_new_nd <- spatial_nd[spatial_nd$SpeciesCode %in% matched_species$SpeciesCode , ]
temporal_new <- temporal[temporal$SpeciesCode %in% matched_species$SpeciesCode ,]


write.csv(matched_species,"species_common_space_time.csv")

write.csv(spatial_new, "spatialdataset_D_R.csv")
write.csv(spatial_new_nd, "spatialdataset_ND_R.csv")
write.csv(temporal_new, "temporaldataset_R.csv")

# make them match before doing this
data <- rbind(spatial_new, temporal_new)
write.csv(data, "wholedataset_D_NOV12.csv")

data_nd <- rbind(spatial_new_nd, temporal_new)
write.csv(data_nd, "wholedataset_ND_NOV12.csv")

