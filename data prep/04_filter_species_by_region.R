setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)
library(openxlsx)
library(readxl)

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


#write.csv(matched_species,"species_common_space_time.csv")

#write.csv(spatial_new, "spatialdataset_D_R.csv")
#write.csv(spatial_new_nd, "spatialdataset_ND_R.csv")
#write.csv(temporal_new, "temporaldataset_R.csv")

# make them match before doing this
data <- rbind(spatial_new, temporal_new)
#write.csv(data, "wholedataset_D_NOV12.csv")

#data_nd <- rbind(spatial_new_nd, temporal_new)
#write.csv(data_nd, "wholedataset_ND_NOV12.csv")



## finding species present in both spatial and temporal sites for each of the 20 regions
t <- vector("list")
s <- vector("list")
species <- vector("list")

s.species <- spatial_new %>% group_by(SpeciesCode, Region) %>%
  summarise_at(vars(Count),list(Count = sum))
t.species <- temporal_new %>% group_by(SpeciesCode, Region) %>%
  summarise_at(vars(Count), list(Count = sum))

t.species <- t.species[which(t.species$Count > 0),]
s.species <- s.species[which(s.species$Count > 0),]

t.species <- select(t.species, -Count)
s.species <- select(s.species,  -Count)

for(i in 1:20) {
  t[[i]] <- filter(t.species, Region == i)
  s[[i]] <- filter(s.species, Region == i)
  
  species[[i]] <- intersect(s[[i]], t[[i]])
}

# writes each list i in species[[i]] to its own sheet in the excel workbook below
write.xlsx(species, file = "matched_species_by_region.xlsx")

spatial_r <- vector("list")
temporal_r <- vector("list")

for(i in 1:20) {
species <- read_excel("matched_species_by_region.xlsx", sheet = i)
s.region <- spatial_new %>% filter(Region == i)
t.region <- temporal_new %>% filter(Region == i)

spatial_r[[i]] <- s.region[s.region$SpeciesCode %in% species$SpeciesCode ,]
temporal_r[[i]] <- t.region[t.region$SpeciesCode %in% species$SpeciesCode ,]
}


spatial_filtered <- rbind(spatial_r[[1]], spatial_r[[2]], spatial_r[[3]], spatial_r[[4]], spatial_r[[5]],
                          spatial_r[[6]], spatial_r[[7]], spatial_r[[8]], spatial_r[[9]],  spatial_r[[10]],
                          spatial_r[[11]], spatial_r[[12]], spatial_r[[13]], spatial_r[[14]], spatial_r[[15]],
                          spatial_r[[16]], spatial_r[[17]], spatial_r[[18]], spatial_r[[19]], spatial_r[[20]])

temporal_filtered <- rbind(temporal_r[[1]], temporal_r[[2]], temporal_r[[3]], temporal_r[[4]], temporal_r[[5]],
                           temporal_r[[6]], temporal_r[[7]], temporal_r[[8]], temporal_r[[9]],  temporal_r[[10]],
                           temporal_r[[11]], temporal_r[[12]], temporal_r[[13]], temporal_r[[14]], temporal_r[[15]],
                           temporal_r[[16]], temporal_r[[17]], temporal_r[[18]], temporal_r[[19]], temporal_r[[20]])

d_filtered <- rbind(spatial_filtered, temporal_filtered)

write.csv(d_filtered,"wholedataset_FILTERED_NOV16.csv")
