
library(tidyverse)

# Load in data
bbs <- read.csv("clean_bbs_dataset.csv", header = T)
ecoregions <- read.csv("routes_ecoregions.csv", header=T)



bbs$rte_id <- as.integer(as.factor(bbs$RouteNumber))

rtes <- distinct(bbs, RouteNumber, rte_id)
nrtes <- length(unique(bbs$rte_id))


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
  if(r$Year[n] == min) {
    rtes$firstyear[i] <- r$Year[n]
    rtes$firstcover[i] <- r$Forest.cover[n]
  }
    
  if(r$Year[n] == max){
    rtes$lastyear[i] <- r$Year[n]
    rtes$lastcover[i] <- r$Forest.cover[n]
  }
  
  else{
    
  }
  
  }
  
}

# Find the change and select for sites with forest cover change >= 20% within the time range (ideally from 2000 to 2019)
rtes$change <- rtes$firstcover - rtes$lastcover
temporal <- rtes[which(rtes$change >= 0.20), ]


# next steps:
# find list of sites with data in 2019
# import buffered dataset shapefile
# filter to spatial sites with data in 2019

# for each temporal site, find a list of spatial sites with data in 2019 that are in the same ecoregion
# for each temporal site, select spatial sites from the list above that are within 100 km (will need to use sp or sf packages and the shapefiles)
# will probably need to write a "for loop" to cycle through each temporal site, and then that will apply the distance criteria
# so a list of 51 spatial site lists for each of the 51 temporal sites 
