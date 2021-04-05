library(tidyverse)
library(openxlsx)
library(readxl)
setwd("~/space-time/data prep")
spatial <- read.csv("spatial_dataset_mar2021_version4.csv")
temporal <- read.csv("temporal_dataset_mar2021_version4.csv")
forestcodes <- read.csv("forestcodes.csv", header = T)
forestcodes <- forestcodes %>% dplyr::select(English_Common_Name, status_forest) %>% distinct(English_Common_Name, status_forest)
bbl <- read.csv("bbl_codes.csv") # had to make manual changes to YRWA, DEJU in Excel as well as add Sooty Grouse, Ruffed Grouse, Northern Bobwhite 
# since BBL doesn't provide codes for gallinaceous birds




# some prep - removing spatial & temporal sites that have <18 spatial matches or bbs years
# leaves me with ? regions
spatial <- spatial %>% filter(!ref == 89909)

temporal <- temporal %>% filter(!ref == 89909)


# ------------------------------------#
#    MATCH SPECIES IN SPACE & TIME    |
# ------------------------------------#

# Find species that are not present at ANY of the spatial sites
s.species <- spatial %>% group_by(English_Common_Name) %>%
summarise_at(vars(Count),list(Count = sum))
s.species <- filter(s.species, !Count == 0)
# Filter for forest birds only
s.species <- merge(s.species, forestcodes, by = "English_Common_Name", all.x = TRUE)
s.species <- s.species %>% filter(status_forest == "F") %>% dplyr::select(-status_forest, -Count)


# Find species that are not present at ANY of the temporal sites
t.species <- temporal %>% group_by(English_Common_Name) %>%
  summarise_at(vars(Count), list(Count = sum))
t.species <- filter(t.species, !Count == 0)
# Filter for forest birds only
t.species <- merge(t.species, forestcodes, by = "English_Common_Name", all.x = TRUE)
t.species <- t.species %>% filter(status_forest == "F") %>% dplyr::select(-status_forest, -Count)


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
data$Region <- as.integer(as.factor(data$Region))
data$SpeciesRegion <- paste(data$BBL, data$Region, sep = "")


## ------------------------------------------------------------------------------------------


#------------------------------#
#    NO DUPLICATES OPTION!!   |
#-----------------------------#


### GENERATING THE NO-DUPLICATES DATASET --- requires manual labour in Excel before continuing!!!!
# get the list of species present in each temporal site (across years) - similar format to the spatial site lists in 02_site_dplyr::selection
t_species <- vector("list")
t_sites <- as.vector(unlist(temporal %>% distinct(RouteNumber)))

for(i in 1:27) {
  temp <- temporal_new %>% filter(RouteNumber == t_sites[i])

  t_species[i] <- temp %>% distinct(BBL)
  
}


n <- 51 # max number of species in a given temporal site (check)
for(i in 1:27){
  df <- unlist(t_species[[i]])
  length(df) <- n
  
  t_species[[i]] <- df
}


species_lists <- mapply(cbind, t_species)
colnames(species_lists) <- t_sites

# export the list - use in Excel to make decisions
write.csv(species_lists, "temp_species_lists_mar2021_version4.csv")

## to get a long-format list that can be searchable in excel
t_species <- vector("list")
for(i in 1:27) {
  temp <- temporal_new %>% filter(RouteNumber == t_sites[i])
  temp <- temp %>% distinct(BBL)
  ref <- t_sites[i]
  
  matrix <- data.frame(temp, ref)
  
  t_species[[i]] <- matrix
  
}

species_list <- do.call("rbind", t_species)

# export the list
write.csv(species_list, "species_lists_long_raw_mar2021_version4.csv")




# get an n_spatial_site (duplicated) by n_species matrix filled with 0-1 indicators - whether that species is present at a given spatial site

# start by loading in the list of non-duplicate spatial sites - no need to re-allocate so remove for now
nodup <- read.csv("nonduplicated_sites_version4.txt", header = T)
nodup <- as.vector(unlist(nodup))

s <- spatial_new %>% distinct(BBL)
rt <- spatial_new %>% distinct(RouteNumber)

# filter out spatial sites that aren't duplicated in the dataset across temporal sites
rt <- rt %>% filter(!RouteNumber %in% nodup)


# turn into simple vectors
species <- as.vector(unlist(s))
routes <- as.vector(unlist(rt))

species <- sort(species)


write.csv(routes, "routes_duplicated_mar2021_version4.csv")
write.csv(species, "species_mar2021_version4.csv")


# create an empty matrix
mat <- matrix(nrow = 94, ncol = 269)

for( i in 1:269 ){
  f <- data %>% filter(RouteNumber == routes[i])
  
  for( n in 1:94 ) {
    if( species[n] %in% f$BBL == TRUE ) {
      
      mat[n, i] <- 1
      
    }else{
      
      mat[n, i] <- 0
      
    }
    
  }
  
  
  
}


write.csv(mat, "species_in_each_route_MASTER_MAR2021_version4.csv")

# note some Excel work will need to be done to set this up - copy & paste species list into a workbook
# copy and transpose -> horizontal the list of spatial sites
# copy and paste the species x spatial sites matrix 
# only focus on re-assigning the 1's in the matrix (those are the species observations at a spatial site)
# will need to cross-check continuously to make sure that when assigning to a temporal site, that temporal site also has that species in at least one of the years
# i.e. refer to the table generated above


# reload in the new matrix (after manual) of BBL-spatial-temporal combos, and convert into a list 
# these are all the species-spatial site-temporal site combos that i'll be considering, chosen to avoid having the same species-level observation as a duplicate
# but still allowing the spatial sites to be duplicated across temporal sites - as long as the same species isn't been analyzed at the spatial site more than once
species_mat <- read.csv("no_dups_species_version4.csv", header = F)

# convert matrix to vector
speciescombo <- as.vector(unlist(species_mat))
speciescombo[which(speciescombo == 0)] <- NA
speciescombo[which(speciescombo == 1)] <- NA

speciescombo <- speciescombo[!is.na(speciescombo)]

# separate data into the space component and create a matching column for species-spatial-temporal
data$indexcombo <- paste(data$BBL, data$RouteNumber, data$ref, sep="-")
spatial <- data %>% filter(space.time == 2)


# filter the space dataset
data_sc <- spatial %>% filter(indexcombo %in% speciescombo)


# load in a list of the non-duplicated spatial sites and their spatial-temporal combos (no species-level separation is required here)
# filter the space dataset separately for these
nodups <- as.vector(unlist(read.csv("nonduplicated_sites_version4.txt", header = T)))

data_nodups <- data %>% filter(RouteNumber %in% nodups)

# rbind the two space datasets together
spatial_new <- rbind(data_sc, data_nodups)


# separate data into the time component
temporal_new <- data %>% filter(space.time == 1)

data <- rbind(spatial_new, temporal_new)

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

t.species <- dplyr::select(t.species, -Count)
s.species <- dplyr::select(s.species,  -Count)

# Find species present (Count > 0) in temporal years and spatial sites for each region
for(i in 1:27) {
  t[[i]] <- filter(t.species, Region == i)
  s[[i]] <- filter(s.species, Region == i)
  
  species[[i]] <- dplyr::intersect(s[[i]], t[[i]])
}


write.xlsx(species, "matched_species_by_region_version4.xlsx")


### Filter the datasets one-by-one by region (can't do it all at once because each region as their own species list)
spatial_r <- vector("list")
temporal_r <- vector("list")

for(i in 1:27) {
  species <- read_excel("matched_species_by_region_version4.xlsx", sheet = i)
  s.region <- spatial_new %>% filter(Region == i)
  t.region <- temporal_new %>% filter(Region == i)
  
  spatial_r[[i]] <- s.region[s.region$BBL %in% species$BBL ,]
  temporal_r[[i]] <- t.region[t.region$BBL %in% species$BBL ,]
}


spatial_filtered <- do.call("rbind", spatial_r)
temporal_filtered <- do.call("rbind", temporal_r)

d_filtered <- rbind(spatial_filtered, temporal_filtered)

d_filtered$Region <- as.integer(as.factor(d_filtered$ref))
d_filtered$SpeciesRegion <- paste(d_filtered$BBL, d_filtered$Region, sep = ".")

#write.csv(d_filtered, "whole_dataset_filteredv1_5p.csv")

# d_filtered <- read.csv("whole_dataset_filteredv1.csv")


# -------------------------------#
#    FURTHER REGION FILTERING    |
#      >50% or 40% presence
# -------------------------------#


### Finding list of species present are more than >50% or >40% of sites or years within their regions

# Find the number of unique routes in each region, in both spatial and temporal datasets
spatial_rc <- d_filtered %>% group_by(space.time, Region) %>% summarise(RouteCount = n_distinct(RouteNumber)) %>% filter(space.time == 2)
temporal_rc <- d_filtered %>% group_by(space.time, Region) %>% summarise(YearCount = n_distinct(Year)) %>% filter(space.time == 1)


# Filter for species present >= 1 at a site or year
df_present <- d_filtered %>% filter(Count >= 1)

# Find number of spatial sites a species is present at within their region
spatial_pc <- df_present %>% group_by(space.time, Region, SpeciesRegion) %>% summarise(SpatialPresent = n_distinct(Transect)) %>% filter(space.time == 2)

# Find number of years a species is present in within their region
temporal_pc <- df_present %>% group_by(space.time, Region, SpeciesRegion) %>% summarise(TemporalPresent = n_distinct(Year)) %>% filter(space.time == 1)

# Merge total route / year counts with present counts
spatial_rcpc <- merge(spatial_rc, spatial_pc, by = "Region")
spatial_rcpc <- dplyr::select(spatial_rcpc, -c(space.time.x, space.time.y, Region))
spatial_rcpc$Prop.Spatial <- (spatial_rcpc$SpatialPresent) / (spatial_rcpc$RouteCount)

temporal_rcpc <- merge(temporal_rc, temporal_pc, by = "Region")
temporal_rcpc <- dplyr::select(temporal_rcpc, -c(space.time.x, space.time.y, Region))
temporal_rcpc$Prop.Temporal <- (temporal_rcpc$TemporalPresent / temporal_rcpc$YearCount)

rcpc <- merge(spatial_rcpc, temporal_rcpc, by = "SpeciesRegion")


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
rcpc_new <- dplyr::select(rcpc, c(SpeciesRegion, over10, over40, over30, over20))


# split the og dataframe again into spatial and temporal
df_final <- merge(d_filtered, rcpc_new, by = "SpeciesRegion")

df_40 <- df_final %>% filter(over40 == 1)
df_30 <- df_final %>% filter(over30 == 1)
df_20 <- df_final %>% filter(over20 == 1)
df_10 <- df_final %>% filter(over10 == 1)

n_distinct(df_40$BBL)
n_distinct(df_30$BBL)
n_distinct(df_20$BBL)
n_distinct(df_10$BBL)


setwd("~/space-time/final datasets")

df_40$Region <- as.integer(as.factor(df_40$ref))
write.csv(df_40, "whole_dataset_ND_version4.csv")


## extra summaries and stuff - helps with ragged array creation
dat <- read.csv("whole_dataset_over40_D.csv")
dat_nd <- read.csv("whole_dataset_over40_ND.csv")

dat_nd$Region <- as.integer(as.factor(dat_nd$ref))
dat_nd$SpeciesRegion <- paste(dat_nd$BBL, dat_nd$Region, sep = "")


s <- dat %>% distinct(BBL, Region)
snd <- dat_nd %>% distinct(BBL, Region)

s <- arrange(s, BBL, Region)

snd <- arrange(snd, BBL, Region)
write.csv(s, "species_sort.csv")
write.csv(snd, "species_sort_nd.csv")

