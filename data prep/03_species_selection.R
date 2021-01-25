library(tidyverse)
library(openxlsx)
library(readxl)

spatial <- read.csv("spatial_dataset.csv")
temporal <- read.csv("temporal_dataset.csv")
forestcodes <- read.csv("forestcodes.csv", header = T)
forestcodes <- forestcodes %>% select(English_Common_Name, status_forest) %>% distinct(English_Common_Name, status_forest)
bbl <- read.csv("bbl_codes.csv") # had to make manual changes to YRWA, DEJU in Excel as well as add Sooty Grouse, Ruffed Grouse, Northern Bobwhite 
# since BBL doesn't provide codes for gallinaceous birds



# some prep - removing spatial & temporal sites
spatial <- spatial %>% filter(!ref == 14130)

temporal <- temporal %>% filter(!ref == 14130)

# ------------------------------------#
#    MATCH SPECIES IN SPACE & TIME    |
# ------------------------------------#

# Find species that are not present at ANY of the spatial sites
s.species <- spatial %>% group_by(English_Common_Name) %>%
summarise_at(vars(Count),list(Count = sum))
s.species <- filter(s.species, !Count == 0)
# Filter for forest birds only
s.species <- merge(s.species, forestcodes, by = "English_Common_Name", all.x = TRUE)
s.species <- s.species %>% filter(status_forest == "F") %>% select(-status_forest, -Count)


# Find species that are not present at ANY of the temporal sites
t.species <- temporal %>% group_by(English_Common_Name) %>%
  summarise_at(vars(Count), list(Count = sum))
t.species <- filter(t.species, !Count == 0)
# Filter for forest birds only
t.species <- merge(t.species, forestcodes, by = "English_Common_Name", all.x = TRUE)
t.species <- t.species %>% filter(status_forest == "F") %>% select(-status_forest, -Count)


# nspecies in spatial = 167, nspecies in temporal = 164 (before filtering for forest)
# find only matched species that appear in both datasets
matched_species <- dplyr::intersect(s.species, t.species) # 149 forest bird species

spatial_new <- spatial %>% filter(English_Common_Name %in% matched_species$English_Common_Name) %>% inner_join(bbl, by = "English_Common_Name")
spatial_new$Region <- as.integer(as.factor(spatial_new$ref))
temporal_new <- temporal %>% filter(English_Common_Name %in% matched_species$English_Common_Name) %>% inner_join(bbl, by = "English_Common_Name")
temporal_new$Region <- as.integer(as.factor(temporal_new$ref))

write.csv(spatial_new, "spatial_dataset_v2.csv")
write.csv(temporal_new, "temporal_dataset_v2.csv")

# Append together
data <- rbind(spatial_new, temporal_new)

# Make species-region index
data$SpeciesRegion <- paste(data$BBL, data$Region, sep = "")


# -------------------------------#
#    SPECIES PRESENT BY REGION   |
# -------------------------------#

## finding species present in both spatial and temporal sites for each of the 53 regions
t <- vector("list")
s <- vector("list")
species <- vector("list")
sp.list <- vector("list")
speciesv2 <- vector("list")

s.species <- spatial_new %>% group_by(BBL, Region) %>%
  summarise_at(vars(Count),list(Count = sum))
t.species <- temporal_new %>% group_by(BBL, Region) %>%
  summarise_at(vars(Count), list(Count = sum))

t.species <- t.species[which(t.species$Count > 0),]
s.species <- s.species[which(s.species$Count > 0),]

t.species <- select(t.species, -Count)
s.species <- select(s.species,  -Count)

# Find species present (Count > 0) in temporal years and spatial sites for each region
for(i in 1:52) {
  t[[i]] <- filter(t.species, Region == i)
  s[[i]] <- filter(s.species, Region == i)
  
  species[[i]] <- dplyr::intersect(s[[i]], t[[i]])
}

# Code to prep for unlisting to summary table
for(i in 1:52){
  dummy <- species[[i]]
  speciesv2[[i]] <- dummy %>% select(-Region)
  sp <- unlist(speciesv2[[i]])
  length(sp) <- 45
  
  sp.list[[i]] <- sp
}

# Saving summary table
matched_region <- mapply(cbind, sp.list)
colnames(matched_region) <- paste("Region", seq(1:52), sep="")
write.csv(matched_region, "matched_species_by_region.csv")
write.xlsx(species, "matched_species_by_region_WORKBOOK.xlsx")


### Filter the datasets one-by-one by region (can't do it all at once because each region as their own species list)
spatial_r <- vector("list")
temporal_r <- vector("list")

for(i in 1:52) {
  species <- read_excel("matched_species_by_region.xlsx", sheet = i)
  s.region <- spatial_new %>% filter(Region == i)
  t.region <- temporal_new %>% filter(Region == i)
  
  spatial_r[[i]] <- s.region[s.region$BBL %in% species$BBL ,]
  temporal_r[[i]] <- t.region[t.region$BBL %in% species$BBL ,]
}


spatial_filtered <- do.call("rbind", spatial_r)
temporal_filtered <- do.call("rbind", temporal_r)

d_filtered <- rbind(spatial_filtered, temporal_filtered)

write.csv(d_filtered, "whole_dataset_filteredv1.csv")

# d_filtered <- read.csv("whole_dataset_filteredv1.csv")


# -------------------------------#
#    FURTHER REGION FILTERING    |
#      >50% or 40% presence
# -------------------------------#


### Finding list of species present are more than >50% or >40% of sites or years within their regions

# Find the number of unique routes in each region, in both spatial and temporal datasets
spatial_rc <- data %>% group_by(space.time, Region) %>% summarise(RouteCount = n_distinct(RouteNumber)) %>% filter(space.time == 2)
temporal_rc <- data %>% group_by(space.time, Region) %>% summarise(YearCount = n_distinct(Year)) %>% filter(space.time == 1)


# Filter for species present >= 1 at a site or year
df_present <- data %>% filter(Count >= 1)

# Find number of spatial sites a species is present at within their region
spatial_pc <- df_present %>% group_by(space.time, Region, SpeciesRegion) %>% summarise(SpatialPresent = n_distinct(Transect)) %>% filter(space.time == 2)

# Find number of years a species is present in within their region
temporal_pc <- df_present %>% group_by(space.time, Region, SpeciesRegion) %>% summarise(TemporalPresent = n_distinct(Year)) %>% filter(space.time == 1)

# Merge total route / year counts with present counts
spatial_rcpc <- merge(spatial_rc, spatial_pc, by = "Region")
spatial_rcpc <- select(spatial_rcpc, -c(space.time.x, space.time.y, Region))
spatial_rcpc$Prop.Spatial <- (spatial_rcpc$SpatialPresent) / (spatial_rcpc$RouteCount)

temporal_rcpc <- merge(temporal_rc, temporal_pc, by = "Region")
temporal_rcpc <- select(temporal_rcpc, -c(space.time.x, space.time.y, Region))
temporal_rcpc$Prop.Temporal <- (temporal_rcpc$TemporalPresent / temporal_rcpc$YearCount)

rcpc <- merge(spatial_rcpc, temporal_rcpc)


## Find speciesregion codes where species present >= 50% of spatial sites and years in that region
nrows <- length(rcpc$SpeciesRegion)
for(i in 1:nrows){
  if(rcpc$Prop.Spatial[i] >= 0.50 & rcpc$Prop.Temporal[i] >= 0.50){
    rcpc$over50[i] <- 1
  }
  else {
    rcpc$over50[i] <- 0
  }
}


# repeat for over 40%
for(i in 1:nrows){
  if(rcpc$Prop.Spatial[i] >= 0.40 & rcpc$Prop.Temporal[i] >= 0.40){
    rcpc$over40[i] <- 1
  }
  else {
    rcpc$over40[i] <- 0
  }
}


write.csv(rcpc, "species_prop40_50.csv")
rcpc_new <- select(rcpc, c(SpeciesRegion, over50, over40))

n_over50 <- sum(rcpc$over50) # 194 species
n_over40 <- sum(rcpc$over40) # 243 species


# split the og dataframe again into spatial and temporal
df_final <- merge(data, rcpc_new, by = "SpeciesRegion")

df_40 <- df_final %>% filter(over40 == 1)

write.csv(df_40, "whole_dataset_speciesover40.csv")

