setwd("/Users/kayla/Documents/space-time/data prep")

library(tidyverse)

sp <- read.csv("SpeciesList.csv")
sp <- sp %>% select(AOU, English_Common_Name)

forestcover <- read.csv("forestcover_master_mar2021.csv", header=T, check.names = FALSE)

ecoregions <- read.csv("routes_ecoregions.csv", header=T)

obs <- read.csv("weather.csv", header=T)


# -------------------------#
#      PREP RAW DATA      | ---------------------------------------------------------------------------------------------
# ------------------------#

# Load in raw 2018 BBS data
d1 <- read.csv("fifty1.csv",header=T)
d2 <- read.csv("fifty2.csv",header=T)
d3 <- read.csv("fifty3.csv",header=T)
d4 <- read.csv("fifty4.csv",header=T)
d5 <- read.csv("fifty5.csv",header=T)
d6 <- read.csv("fifty6.csv",header=T)
d7 <- read.csv("fifty7.csv",header=T)
d8 <- read.csv("fifty8.csv",header=T)
d9 <- read.csv("fifty9.csv",header=T)
d10  <- read.csv("fifty10.csv",header=T)

# Rename column in d8 so column names match across dataframes
names(d8)[names(d8)=="statenum"] <- "StateNum"

# Create one dataframe
d <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10)

# Create unique placeholder ID for statenum + route
d$placeholder <- paste(d$StateNum, d$Route, sep=".")

# Create another df to get a list of unique Route & StateNum's
r <- distinct(d, StateNum, Route, .keep_all = TRUE)
r <- select(r, -c(5:57))
r <- select(r, -2)

# Convert Route to a RouteNum that combines State/Province number and the individual Route within the state
# i.e. Statenum = 04 and Route = 001 becomes RouteNumber = 4001
# StateNum goes up to 2 digits and Route goes up to 3 digits
nrows <- length(r$Route)

for(i in 1:nrows) {
  print(paste0("Progress: ", round(i/nrows*100, 2), "% finished."))
  if(r$Route[i] < 100 & r$Route[i] >= 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="0")
  } 
  else if(r$Route[i] < 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="00")
  }
  else {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="")
  }
}

r <- select(r, -c(RouteDataID, Route, StateNum))
d <- merge(d, r, by = "placeholder")

# Create unique transect column
d$Transect <- paste(d$RouteNumber, d$Year, sep=".")
d <- select(d, -c(RPID, placeholder))
d <- select(d, -c(18:56)) # delete stops 12 to 50 

# Match common names by AOU
d <- merge(d, sp, by="AOU")

write.csv(d, "prep_raw_data.csv")




# -------------------------#
#    CLEAN UP SPECIES     | ---------------------------------------------------------------------------------------------
# ------------------------#

d <- read.csv("prep_raw_data.csv")


# Merge subspecies into 1 species for northern flicker, dark-eyed junco and yellow-rumped warbler
d$English_Common_Name <- gsub(".+? Yellow-rumped Warbler", "Yellow-rumped Warbler", d$English_Common_Name)
d$English_Common_Name <- gsub(".+? Northern Flicker", "Northern Flicker", d$English_Common_Name)
d$English_Common_Name <- gsub(".+? Dark-eyed Junco", "Dark-eyed Junco", d$English_Common_Name)
d$English_Common_Name <- gsub("African Collared Dove .+?", "African Collared Dove", d$English_Common_Name)
d$English_Common_Name <- gsub(".+? Great Blue Heron", "Great Blue Heron", d$English_Common_Name)
d$English_Common_Name<- gsub("Black Brant", "Brant", d$English_Common_Name)

# Remove unid. observations and hybrids
## dd = 3607797 obs
dd <- d %>%  filter(!grepl('unid.|hybrid', English_Common_Name))

# move into long format
dd_long <- reshape(dd, v.names = "Count", varying = 8:18, timevar = "Stop", times = names(dd)[8:18], direction='long')

dd_long$Stop <- str_remove(dd_long$Stop, "Stop")
write.csv(dd_long, "check_dd_long.csv")

# -----------------------------#
#    US &CANADA FILTERING     | ------------------------------------------------------------------------------------------
# ----------------------------#
# Load csv file back in
# dd_long <- read.csv("check_dd_long.csv")
# 
# 


ddf <- read.csv("~/space-time/data prep/dd_long_all_FINAL.csv")


## IMPORTANT STEP!
# Because of renaming above under "clean up species", counts for the same species can appear twice but not in the same record
# Northern Flicker, Yellow-Rumped Warbler, and Dark-eyed Junco might have more than 1 record per stop

# Group by my columns down to the stop-level and summarize the counts 
#ddf <- read.csv("dd_long_open.csv")

ddf <- ddf %>% group_by(Transect, RouteNumber, Year, CountryNum, English_Common_Name, BBL, Stop) %>% summarize(Count = sum(Count))

# Now repeat across all 11 stops calcualting species abundance
ddf <- ddf %>% group_by(Transect, RouteNumber, Year, CountryNum, English_Common_Name, BBL) %>% summarize(Count = sum(Count))

# Calculate total species richness
summarize_df <- ddf %>% group_by(Transect, RouteNumber, Year, CountryNum) %>% summarize(Richness = n_distinct(which(Count >= 1)))


summarize_df_clean <- summarize_df %>% filter(!NumForestStops == 0)

write.csv(summarize_df_clean, "~/space-time/data prep/SR1_total/summarize_df.csv")

#### OBSERVER AND WEATHER  ---------------------------------------------------------------------
# Create unique placeholder ID for statenum + route
obs <- read.csv("weather.csv", header=T)

obs$placeholder <- paste(obs$StateNum, obs$Route, sep=".")

# Create another df to get a list of unique Route & StateNum's
r <- distinct(obs, StateNum, Route, .keep_all = TRUE)
r <- dplyr::select(r, -c(5:21))
r <- dplyr::select(r, -2)

# Convert Route to a RouteNum that combines State/Province number and the individual Route within the state
# i.e. Statenum = 04 and Route = 001 becomes RouteNumber = 4001
# StateNum goes up to 2 digits and Route goes up to 3 digits
nrows <- length(r$Route)

for(i in 1:nrows) {
  print(paste0("Progress: ", round(i/nrows*100, 2), "% finished."))
  if(r$Route[i] < 100 & r$Route[i] >= 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="0")
  } 
  else if(r$Route[i] < 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="00")
  }
  else {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="")
  }
  
}

r <- dplyr::select(r, -c(RouteDataID, StateNum, Route, RunType))
obs <- merge(obs, r, by = "placeholder", all.x = FALSE)
obs$Transect <- paste(obs$RouteNumber, obs$Year, sep=".")

obs_id <- as.vector(unlist(obs %>% distinct(ObsN)))


obs_list <- vector("list")
for( i in obs_id ){
  # filter for all observations done by an observer in the whole history of bbs
  o <- obs %>% filter(ObsN == i )
  nrow <- nrow(o)
  
  min <- min(o$Year)
  max <- max(o$Year)
  
  for( n in 1:nrow ){
    
    # find the first year the observer surveyed for bbs and mark it as a first observation
    if( o$Year[n] == min ){
      o$FirstObs[n] = 2
    }else{
      o$FirstObs[n] = 1
    }
  
  }
  
  obs_list[[paste(i)]] <- o
}


obs <- do.call("rbind", obs_list)

obs_clean <- obs %>% dplyr::select(c(Transect, ObsN, StartWind, RunType, FirstObs))
obs_clean <- obs_clean[!duplicated(obs_clean$Transect), ]

# Select years >= 2000
summarize_df$Transect <- paste(summarize_df$RouteNumber, summarize_df$Year, sep=".")
ddf <- merge(summarize_df, obs_clean, by = "Transect", all.x = FALSE)
ddf <- ddf %>% filter(Year >= 2000)

write.csv(ddf, "~/space-time/data prep/SR1_total/base_100m_dataset_total.csv")



## Filter for the sites I want across the 27 comparison regions (pre-identified)

# Re-load in the base dataset generated above
dddf <- read.csv("~/space-time/data prep/SR1_total/base_100m_dataset_total.csv")
dddf$Transect <- paste(dddf$RouteNumber, dddf$Year, sep=".")


###### Add in forest cover #########
forestcover <- read.csv("~/space-time/data prep/Forest cover/forestcover_master_mar2021.csv", header=T, check.names = FALSE)

change <- forestcover %>% dplyr::select(rte, change)
cover <- dplyr::select(forestcover, -c(change))

# reformat to long
forest_long <- reshape(cover, v.names="Forest cover", varying = 2:21, timevar="Year", times=names(cover)[2:21],direction='long')
forest_long$Transect <- paste(forest_long$rte, forest_long$Year, sep=".")
forest_long <- dplyr::select(forest_long, -c(rte, Year, id))

# merge forest
dddff <- merge(dddf, forest_long, by = "Transect")

write.csv(dddff, "whole_dataset_apr2021.csv") # whole dataset move onto 02_site_selection

# then once you've got the sites, use the resulting total SR dataset to filter subsequent metrics




