library(sp)
library(sf)
library(rgdal)
library(tidyverse)
library(rgeos)
library(openxlsx)
library(rlist)

setwd("/Users/kayla/Documents/space-time/data prep")
# Load in data
bbs <- read.csv("whole_dataset_apr2021.csv", header = T)
bbs$Transect <- paste(bbs$RouteNumber, bbs$Year, sep = ".")
ecoregions <- read.csv("routes_ecoregions.csv", header=T)
shp <- st_read("C:/Users/kayla/Documents/arcmap/buffer_dataset_1km.shp")


# Set up index for routes
bbs$rte_id <- as.integer(as.factor(bbs$RouteNumber))
bbs <- merge(bbs, ecoregions, by = "RouteNumber")


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


temporal_years <- bbs %>% filter(RouteNumber %in% temporal_loss$RouteNumber)
temporal_years <- temporal_years %>% group_by(RouteNumber) %>% summarize(nyears = n_distinct(Year))
temporal_years <- temporal_years %>% filter(nyears >= 15)

temporal_years2 <- bbs %>% filter(RouteNumber %in% temporal_gain$RouteNumber)
temporal_years2 <- temporal_years2 %>% group_by(RouteNumber) %>% summarize(nyears = n_distinct(Year))
temporal_years2 <- temporal_years2 %>% filter(nyears >= 15)

temporal_loss$range <- temporal_loss$lastyear - temporal_loss$firstyear
temporal_gain$range <- temporal_gain$lastyear - temporal_gain$firstyear

# select for sites with at least 15 years of data in the bbs and cover at least 2000 to 2018
temporal_loss <- temporal_loss %>% filter(range == 19) %>% filter(RouteNumber %in% temporal_years$RouteNumber)
temporal_gain <- temporal_gain %>% filter(range == 19) %>% filter(RouteNumber %in% temporal_years$RouteNumber)


temporal <- temporal_loss


## SPATIAL SITE SELECTION -------------------------------------------------------------------------------------
# Find bbs routes with data in 2019 and omit all the temporal sites identified above
sp2019 <- bbs[which(bbs$Year == 2019) , ]
sp2019 <- sp2019 %>% filter(!RouteNumber %in% temporal$RouteNumber)
sp2019 <- distinct(sp2019, RouteNumber, Year, Forest.cover, Ecoregion_L1Code, Ecoregion_L1Name)

ntemp <- nrow(temporal_loss)
spEco.list <- vector("list")
spEco.list <- vector("list")

# Find lists of routes that are in the same ecoregion as each temporal route and fall within the same % forest cover range
for(i in 1:64) {
  tempSite <- temporal_loss[i,]
  tempEco <- tempSite$Ecoregion_L1Code
  
  # Select for sites that fall in the same ecoregion and fall in the same forest cover gradient established by the temporal site's first and last year forest cover
  # give or take 5% - lower gives me not a lot to work with and still probably represents forest cover gradient well; 10% was prob too big
  spEco.list[[paste(tempSite$RouteNumber)]] <- sp2019 %>% filter(Ecoregion_L1Code == tempEco) %>% filter(Forest.cover <= (tempSite$firstcover+0.05)) %>% filter(Forest.cover >= (tempSite$lastcover-0.05))
  
}



sites <- temporal$RouteNumber
write.csv(sites, "list_of_temporal_sites_version5.csv")



# Convert route shapefile into a spatial feature layer - allows us to use dpylr:: functions on attribute table
shpSF <- st_as_sf(shp)

spDist.list <- vector("list")
buff <- st_read("C:/Users/kayla/Documents/arcmap/april 2021/300km_buffer_temporal_64.shp")
buff_sf <- st_as_sf(buff)

for(i in sites){

  # Get buffer for that temporal site
  tempBuff <- buff_sf %>% filter(rteno == i)
  tempBuff <- as(tempBuff, "Spatial")
  

  if(nrow(spEco.list[[paste(i)]]) > 0){
# Extract list of spatial candidates for the temporal site (same ecoregion)

  spDF <- data.frame(spEco.list[[paste(i)]])
  spList <- spDF$RouteNumber
  spShp_sf <- shpSF %>% filter(rteno %in% spList)
  
  
  # Convert back to spatial object
  spShp <- as(spShp_sf, "Spatial")
  
  # F1nd spatial candidates that fall within the 300 km distance
  dist <- spShp[tempBuff,]

  plot(dist)
  plot(tempBuff, add=TRUE)
  mtext(paste(i))
  
  
  if(nrow(st_as_sf(dist)) > 0) {
    dist_sf <- st_as_sf(dist)
    dist_sf$ref <- i
    dist_df <- data.frame(dist_sf) %>% dplyr::select(rteno, ref)
    spDist.list[[paste(i)]] <- dist_df
  }
  
  }else{
    spDist.list[[paste(i)]] <- "no matches!"
  
}
}
  


## get summary table
sp.list <- vector("list")


for(i in sites){
  dummy <- data.frame(spDist.list[[paste(i)]])
  
  if(nrow(dummy) >= 15) {
  dummy <- dummy$rteno
  }
  else{
  dummy <- "less than 15!"
  }
  
  sp.list[[paste(i)]] <- dummy
}


# n = length of longest list in spDist.list (replace as needed if changing criteria above)
n <- 67
for(i in sites){
  df <- unlist(sp.list[[paste(i)]])
  length(df) <- n
  
  sp.list[[paste(i)]] <- df
}



# Cbind list of lists 
final <- mapply(cbind, sp.list)
write.csv(final, "spatial_candidates_apr2021.csv")


# Long format
final <- do.call("rbind", spDist.list)

# These are the sites with <15 spatial site matches
remove <- c(4078, 11407, 14018, 14156, 14186, 14953, 25135, 42032, 53900, 63902, 69029,
            69201, 80101, 81050, 83347, 89004, 89025, 89079, 89909)
final <- final %>% dplyr::filter(!ref %in% remove)
write.csv(final, "spatial_candidates_apr2021_long.csv")

### filter datasets
spatial <- do.call("rbind", spDist.list)
spatial$Transect <- paste(spatial$rteno, "2019", sep=".")
spatial_merge <- base::merge(spatial, bbs, by = "Transect", all.x = FALSE)
spatial_merge$space.time <- rep(2)

spatial_merge <- spatial_merge %>% filter(!ref %in% remove)
write.csv(spatial_merge, "spatial_dataset_apr2021.csv")


temp <- temporal$RouteNumber
temporal_f <- bbs %>% filter(RouteNumber %in% temp)
temporal_f$space.time <- rep(1)

temporal_f <- temporal_f %>% filter(!RouteNumber %in% remove)

write.csv(temporal_f, "temporal_dataset_apr2021.csv")




# rbind and save the total SR dataset

s <- read.csv("spatial_dataset_apr2021.csv")

t <- read.csv("temporal_dataset_apr2021.csv")

richness <- rbind(s, t)

write.csv(richness, "~/space-time/final datasets/SR1_total/total_richness_FINAL.csv")
