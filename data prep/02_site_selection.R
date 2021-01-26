library(sp)
library(sf)
library(rgdal)
library(tidyverse)
library(rgeos)
library(openxlsx)
library(rlist)

setwd("/Users/kayla/Documents/space-time/data prep")
# Load in data
bbs <- read.csv("clean_bbs_dataset.csv", header = T)
bbs$Transect <- paste(bbs$RouteNumber, bbs$Year, sep = ".")
ecoregions <- read.csv("routes_ecoregions.csv", header=T)
shp <- readOGR("buffer_dataset_1km_proj_V3.shp")


# Set up index for routes
bbs$rte_id <- as.integer(as.factor(bbs$RouteNumber))

# Get a list of all distinct bbs routes
rtes <- distinct(bbs, RouteNumber, rte_id, Ecoregion_L1Code, Ecoregion_L1Name)
nrtes <- length(unique(bbs$rte_id))




## TEMPORAL SITE SELECTION ---------------------------------------------------------------------------------------


# Find the new time ranges from 2000 to 2019 for routes after accounting for years with BBS data
# and the corresponding forest change that occurred in that new time range
for(i in 1:nrtes) {
  print(paste0("Progress: ", round(i/nrtes*100, 2), "% finished."))
  r <- bbs[which(bbs$rte_id == i),]
  n <- nrow(r)
  
  
  # Find minimum and maximum years, i.e. time range for that route
  min <- min(r$Year)
  max <- max(r$Year)
  
  for(n in 1:n) {
    
    # If the year on iteration n is the minimum (earliest) year for that route, then copy over its % forest cover
  if(r$Year[n] == min) {
    rtes$firstyear[i] <- r$Year[n]
    rtes$firstcover[i] <- r$Forest.cover[n]
  }
    # If the year on iteration n is the maximum (latest) year for that route, then copy over its % forest cover
  if(r$Year[n] == max){
    rtes$lastyear[i] <- r$Year[n]
    rtes$lastcover[i] <- r$Forest.cover[n]
  }
  
  else{
    
  }
  
  }
  
}

# Find the change in % forest between first and last years with bbs data
rtes$change <- rtes$firstcover - rtes$lastcover

# select for sites with forest cover change >= 20% within the time range (ideally from 2000 to 2019)
temporal_loss <- rtes[which(rtes$change >= 0.20), ]
temporal_gain <- rtes[which(rtes$change <= -0.20),]


temporal <- rbind(temporal_loss, temporal_gain)
temporal$nyears <- temporal$lastyear - temporal$firstyear


## SPATIAL SITE SELECTION -------------------------------------------------------------------------------------
# Find bbs routes with data in 2019
sp2019 <- bbs[which(bbs$Year == 2019) , ]
sp2019 <- distinct(sp2019, RouteNumber, Year, Forest.cover, Ecoregion_L1Code, Ecoregion_L1Name)

ntemp <- nrow(temporal_loss)
spEco.list <- vector("list")
spEco.list <- vector("list")

# Find lists of routes that are in the same ecoregion as each temporal route and fall within the same % forest cover range
for(i in 1:51) {
  tempSite <- temporal_loss[i,]
  tempEco <- tempSite$Ecoregion_L1Code
  
  # Select for sites that fall in the same ecoregion and fall in the same forest cover gradient established by the temporal site's first and last year forest cover
  # give or take 10% for now because candidate turnout was so low ... will need to put more thought into this threshold
  spEco.list[[i]] <- sp2019 %>% filter(Ecoregion_L1Code == tempEco) %>% filter(Forest.cover <= (tempSite$firstcover+0.05)) %>% filter(Forest.cover >= (tempSite$lastcover-0.05))
  
}

spEco.list_gain <- vector("list")

for(i in 1:2) {
  tempSite <- temporal_gain[i,]
  tempEco <- tempSite$Ecoregion_L1Code
  
  # Select for sites that fall in the same ecoregion and fall in the same forest cover gradient established by the temporal site's first and last year forest cover
  # give or take 10% for now because candidate turnout was so low ... will need to put more thought into this threshold
  spEco.list_gain[[i]] <- sp2019 %>% filter(Ecoregion_L1Code == tempEco) %>% filter(Forest.cover >= (tempSite$firstcover-0.05)) %>% filter(Forest.cover <= (tempSite$lastcover+0.05))
  
}

spEco.list <- c(spEco.list, spEco.list_gain)

# Convert route shapefile into a spatial feature layer - allows us to use dpylr:: functions on attribute table
shpSF <- st_as_sf(shp)
shpSF <- st_transform(shpSF, crs = "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs") # reproject to North America Lambert Conic


spDist.list <- vector("list")

for(i in 1:53) {
  
  # Extract the shapefile point of the temporal route i 
  tempShp_sf <- shpSF %>% filter(rteno == temporal$RouteNumber[i])
  # Convert back to spatial object
  tempShp <- as(tempShp_sf, "Spatial")
  
  # Buffer by 300 km around temporal site (distance criterion)
  tempBuff <- gBuffer(tempShp, width = 300000) # 100 km, or can do 200 km or 300 km
  
  # Extract list of spatial candidates for the temporal site (same ecoregion)
  if(nrow(spEco.list[[i]]) > 0) {
  spDF <- data.frame(spEco.list[[i]])
  spList <- spDF$RouteNumber
  spShp_sf <- shpSF %>% filter(rteno %in% spList)
  # Convert back to spatial object
  spShp <- as(spShp_sf, "Spatial")
  
  # Find spatial candidates that fall within the 100 km distance
  dist <- gContains(tempBuff, spShp, byid = TRUE)
  distDf <- data.frame(spList, dist)
  
  
  # Final list of spatial route candidates
  distDf <- distDf %>% filter(buffer == "TRUE") %>% dplyr::select(spList)
  
  if(nrow(distDf) > 0) {
  distDf$ref <- temporal$RouteNumber[i]
  spDist.list[[i]] <- distDf
  }
  
  spDist.list[[i]] <- distDf
  
  }
  
  else {
    spDist.list[[i]] <- paste("need to remove!")
  }
}


## get summary table
sp.list <- vector("list")

for(i in 1:53){
  dummy <- spDist.list[[i]]
  
  if(nrow(dummy) > 0) {
  dummy <- dummy %>% select(-ref)
  }
  
  sp.list[[i]] <- dummy
}


# n = length of longest list in spDist.list (replace as needed if changing criteria above)
n <- 88
for(i in 1:53){
  df <- unlist(sp.list[[i]])
  length(df) <- n
  
  sp.list[[i]] <- df
}

# Cbind list of lists 
final <- mapply(cbind, sp.list)
# Rename columns to be the temporal route number
colnames(final) <- temporal$RouteNumber

write.csv(final, "spatial_candidates_300km_5p.csv")



### filter datasets
spatial <- do.call("rbind", spDist.list)
spatial$Transect <- paste(spatial$spList, "2019", sep=".")
spatial <- select(spatial, -c(spList))
spatial_merge <- merge(spatial, bbs, by = "Transect", all.x = FALSE)
spatial_merge$space.time <- rep(2)
write.csv(spatial_merge, "spatial_dataset_5p.csv")


temp <- temporal$RouteNumber
temporal_f <- bbs %>% filter(RouteNumber %in% temp)
temporal_f$space.time <- rep(1)

write.csv(temporal_f, "temporal_dataset_5p.csv")


