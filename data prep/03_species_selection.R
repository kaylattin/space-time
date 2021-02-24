library(tidyverse)
library(openxlsx)
library(readxl)
setwd("~/space-time/data prep")
spatial <- read.csv("spatial_dataset_5p.csv")
temporal <- read.csv("temporal_dataset_5p.csv")
forestcodes <- read.csv("forestcodes.csv", header = T)
forestcodes <- forestcodes %>% select(English_Common_Name, status_forest) %>% distinct(English_Common_Name, status_forest)
bbl <- read.csv("bbl_codes.csv") # had to make manual changes to YRWA, DEJU in Excel as well as add Sooty Grouse, Ruffed Grouse, Northern Bobwhite 
# since BBL doesn't provide codes for gallinaceous birds




# some prep - removing spatial & temporal sites that have <10 spatial matches or bbs years
# leaves me with 40 regions!
remove <- c(4078, 11256, 4116, 4141, 6073, 11057, 11234, 11407, 11410, 14013, 14018, 14130, 14132, 14136, 14156, 14186, 81050, 68373, 53900)
spatial <- spatial %>% filter(!ref %in% remove)

temporal <- temporal %>% filter(!ref %in% remove)

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

#write.csv(spatial_new, "spatial_dataset_v2_5p.csv")
#write.csv(temporal_new, "temporal_dataset_v2_5p.csv")

# Append together
data <- rbind(spatial_new, temporal_new)

# Make species-region index
data$SpeciesRegion <- paste(data$BBL, data$Region, sep = "")



#------------------------------#
#    NO DUPLICATES OPTION!!   |
#-----------------------------#


### GENERATING THE NO-DUPLICATES DATASET --- requires manual labour in Excel before continuing!!!!
# get the list of species present in each temporal site (across years) - similar format to the spatial site lists in 02_site_selection


# export the list - use in Excel to make decisions





# get an n_spatial_site (duplicated) by n_species matrix filled with 0-1 indicators - whether that species is present at a given spatial site

# start by loading in the list of non-duplicate spatial sites to remove
nodup <- read.csv("nonduplicated_sites.txt", header = T)
nodup <- as.vector(unlist(nodup))

s <- spatial_new %>% distinct(BBL)
rt <- spatial_new %>% distinct(RouteNumber)

# filter out spatial sites that aren't duplicated in the dataset across temporal sites
rt <- rt %>% filter(!RouteNumber %in% nodup)


# turn into simple vectors
species <- as.vector(unlist(s))
routes <- as.vector(unlist(rt))


write.csv(routes, "routes_duplicated_feb25.csv")
write.csv(species, "species_feb25.csv")


# create an empty matrix
mat <- matrix(nrow = 58, ncol = 244)

for( i in 1:244 ){
  f <- df_40 %>% filter(RouteNumber == routes[i])
  
  for( n in 1:58 ) {
    if( species[n] %in% f$BBL == TRUE ) {
      
      mat[n, i] <- 1
      
    }else{
      
      mat[n, i] <- 0
      
    }
    
  }
  
  
  
}


write.csv(mat, "species_in_each_route_MASTER_FEB25.csv")

# note some Excel work will need to be done to set this up - copy & paste species list into a workbook
# copy and transpose -> horizontal the list of spatial sites
# copy and paste the species x spatial sites matrix 
# only focus on re-assigning the 1's in the matrix (those are the species observations at a spatial site)
# will need to cross-check continuously to make sure that when assigning to a temporal site, that temporal site also has that species in at least one of the years
# i.e. refer to the table generated above


# reload in the new matrix (after manual) of BBL-spatial-temporal combos, and convert into a list 
# these are all the species-spatial site-temporal site combos that i'll be considering, chosen to avoid having the same species-level observation as a duplicate
# but still allowing the spatial sites to be duplicated across temporal sites - as long as the same species isn't been analyzed at the spatial site more than once




# separate data into the space component and create a matching column for species-spatial-temporal



# filter the space dataset




# load in a list of the non-duplicated spatial sites and their spatial-temporal combos (no species-level separation is required here)
# filter the space dataset separately for these





# rbind the two space datasets together




# separate data into the time component
temporal_new <- data %>% filter(space.time = 1)






# continue below to identifying species present by comparison region
# AND applying the 40% presence threshold criteria, although may need to ease up on that % if there are barely any species left to work with




#### END OF R CODE TO GENERATE THE NON-DUPLICATED DATASET --------------------------------------------------------------------


 


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
for(i in 1:35) {
  t[[i]] <- filter(t.species, Region == i)
  s[[i]] <- filter(s.species, Region == i)
  
  species[[i]] <- dplyr::intersect(s[[i]], t[[i]])
}

# Code to prep for unlisting to summary table
for(i in 1:35){
  dummy <- species[[i]]
  speciesv2[[i]] <- dummy %>% select(-Region)
  sp <- unlist(speciesv2[[i]])
  length(sp) <- 45
  
  sp.list[[i]] <- sp
}

# Saving summary table
matched_region <- mapply(cbind, sp.list)
colnames(matched_region) <- paste("Region", seq(1:35), sep="")
#write.csv(matched_region, "matched_species_by_region_5p.csv")
write.xlsx(species, "matched_species_by_region_WORKBOOK_5p.xlsx")


### Filter the datasets one-by-one by region (can't do it all at once because each region as their own species list)
spatial_r <- vector("list")
temporal_r <- vector("list")

for(i in 1:35) {
  species <- read_excel("matched_species_by_region_WORKBOOK_5p.xlsx", sheet = i)
  s.region <- spatial_new %>% filter(Region == i)
  t.region <- temporal_new %>% filter(Region == i)
  
  spatial_r[[i]] <- s.region[s.region$BBL %in% species$BBL ,]
  temporal_r[[i]] <- t.region[t.region$BBL %in% species$BBL ,]
}


spatial_filtered <- do.call("rbind", spatial_r)
temporal_filtered <- do.call("rbind", temporal_r)

d_filtered <- rbind(spatial_filtered, temporal_filtered)

#write.csv(d_filtered, "whole_dataset_filteredv1_5p.csv")

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


## Find speciesregion codes where species present >= 40% of spatial sites and years in that region
nrows <- length(rcpc$SpeciesRegion)
for(i in 1:nrows){
  if(rcpc$Prop.Spatial[i] >= 0.40 & rcpc$Prop.Temporal[i] >= 0.40){
    rcpc$over40[i] <- 1
  }
  else {
    rcpc$over40[i] <- 0
  }
}


# repeat for over 30%
for(i in 1:nrows){
  if(rcpc$Prop.Spatial[i] >= 0.30 & rcpc$Prop.Temporal[i] >= 0.30){
    rcpc$over30[i] <- 1
  }
  else {
    rcpc$over30[i] <- 0
  }
}

# repeat for over 20%
for(i in 1:nrows){
  if(rcpc$Prop.Spatial[i] >= 0.20 & rcpc$Prop.Temporal[i] >= 0.20){
    rcpc$over20[i] <- 1
  }
  else {
    rcpc$over20[i] <- 0
  }
}


# repeat for over 10%
for(i in 1:nrows){
  if(rcpc$Prop.Spatial[i] >= 0.10 & rcpc$Prop.Temporal[i] >= 0.10){
    rcpc$over10[i] <- 1
  }
  else {
    rcpc$over10[i] <- 0
  }
}


#write.csv(rcpc, "species_prop40_to_10.csv")
rcpc_new <- select(rcpc, c(SpeciesRegion, over10, over40, over30, over20))

n_over40 <- sum(rcpc$over40) # 310 speciesregions
n_over30 <- sum(rcpc$over30) # 405 speciesregions
n_over20 <- sum(rcpc$over20) # 405 speciesregions
n_over10 <- sum(rcpc$over10) # 405 speciesregions

# split the og dataframe again into spatial and temporal
df_final <- merge(data, rcpc_new, by = "SpeciesRegion")

df_40 <- df_final %>% filter(over40 == 1)
df_30 <- df_final %>% filter(over30 == 1)
df_20 <- df_final %>% filter(over20 == 1)
df_10 <- df_final %>% filter(over10 == 1)

n_distinct(df_40$BBL)
n_distinct(df_30$BBL)
n_distinct(df_20$BBL)
n_distinct(df_10$BBL)







write.csv(df_40, "whole_dataset_over40_5p - FEB 23.csv")
#write.csv(df_30, "whole_dataset_over30_5p.csv")
#write.csv(df_10, "whole_dataset_over10_5p.csv")

tally <- vector("list")

df_10$Region <- as.integer(as.factor(df_10$ref))



## extra summaries and stuff

for(i in 1:37) {
  region <- df_10 %>% filter(Region == i)
  
  tally[[i]] <- n_distinct(region$BBL)
  
}

t <- mapply(cbind, tally)
sum(t)





s <- df_40 %>% distinct(BBL)
r <- df_40 %>% filter(space.time == 2) %>% distinct(RouteNumber)
rt <- df_40 %>% filter(space.time == 2) %>% distinct(RouteNumber)

species <- as.vector(unlist(s))
routes <- as.vector(unlist(r))

write.csv(routes, "routes_feb23.csv")
write.csv(species, "species_feb23.csv")


mat <- matrix(nrow = 58, ncol = 432)

for( i in 1:432 ){
  f <- df_40 %>% filter(RouteNumber == routes[i])
  
  for( n in 1:58 ) {
    if( species[n] %in% f$BBL == TRUE ) {
      
      mat[n, i] <- 1
      
    }else{
      
      mat[n, i] <- 0
      
    }
    
  }
  
  
  
}


write.csv(mat, "species_in_each_route.csv")

