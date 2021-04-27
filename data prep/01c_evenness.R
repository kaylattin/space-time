setwd("/Users/kayla/Documents/space-time/data prep")

library(tidyverse)
library(vegan)

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

# -------------------------------------------#
#   SPECIES diversity ACROSS FORESTED STOPS   | ------------------------------------------------------------------------------------------
# ------------------------------------------#
# Load csv file back in
dd_long <- read.csv("check_dd_long.csv")



forestcodes <- read.csv("forestcodes_REVISED.csv", header = T)
forestcodes <- forestcodes %>% dplyr::select(English_Common_Name, new.status) %>% distinct(English_Common_Name, new.status)
bbl <- read.csv("bbl_codes.csv") # had to make manual changes to YRWA, DEJU in Excel as well as add Sooty Grouse, Ruffed Grouse, Northern Bobwhite 
# since BBL doesn't provide codes for gallinaceous birds

ddf <- merge(dd_long, bbl, by = "English_Common_Name", all.x=FALSE)
ddf <- merge(ddf, forestcodes, by = "English_Common_Name")

ddf2 <- ddf %>% filter(!new.status == "W")  ## all except waterbirds, shorebirds, nocturnal
ddf2 <- ddf2 %>% filter(!new.status == "N")

# Write to csv file
write.csv(ddf, "~/space-time/data prep/dd_long_all_FINAL.csv")

ddf <- read.csv("~/space-time/data prep/dd_long_all_FINAL.csv")


#IMPORTANT STEP!
# Because of renaming above under "clean up species", counts for the same species can appear twice but not in the same record
# Northern Flicker, Yellow-Rumped Warbler, and Dark-eyed Junco might have more than 1 record per stop

# Group by my columns down to the stop-level and summarize the counts 
ddf$Transect <- paste(ddf$RouteNumber, ddf$Year, sep=".")
ddf <- ddf %>% group_by(Transect, RouteNumber, Year, CountryNum, English_Common_Name, BBL, Stop) %>% summarize(Count = sum(Count))

# Find species abundance for each route, each species at all stops
abund <- ddf %>% group_by(Transect, RouteNumber, Year, CountryNum, English_Common_Name, BBL) %>% summarize(Count = sum(Count))

# THEN calculate richness and shannon
shannon <- abund %>% group_by(Transect, RouteNumber, Year, CountryNum) %>% summarize(Div = diversity(Count, index = "shannon"))
richness <-  abund %>% group_by(Transect, RouteNumber, Year, CountryNum) %>% summarize(Richness = n_distinct(which(Count >= 1)))

# Select out the shannon column
#shannon$id <- paste(shannon$RouteNumber, shannon$Year, shannon$Stop, sep=".")

#shannon <- shannon[,c(6,7)]
#richness$id <- paste(richness$RouteNumber, richness$Year, richness$Stop, sep=".")

# Merge with richness dataframe
summarize_df <- merge(shannon, richness, by = "Transect")

write.csv(summarize_df, "shannon_richness.csv")

# Calculate evenness for each stop
summarize_df$evenness = summarize_df$Div / log(summarize_df$Richness)

summarize_df[is.na(summarize_df)] <- 0


# Write
write.csv(summarize_df, "~/space-time/data prep/summarize_df_evenness_FINAL.csv")


# -----------------------------#
#    OBSERVERS & WEATHER       | ------------------------------------------------------------------------------------------
# ----------------------------#

# Create unique placeholder ID for statenum + route
obs <- read.csv("~/space-time/data prep/weather.csv", header=T)

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
  # Filter for all observations done by an observer in the whole history of bbs
  o <- obs %>% filter(ObsN == i )
  nrow <- nrow(o)
  
  min <- min(o$Year)
  max <- max(o$Year)
  
  for( n in 1:nrow ){
    
    # Find the first year the observer surveyed for bbs and mark it as a first observation
    if( o$Year[n] == min ){
      o$FirstObs[n] = 2
    }else{
      o$FirstObs[n] = 1
    }
    
  }
  
  obs_list[[paste(i)]] <- o
}

# Rbind into a dataframe
obs <- do.call("rbind", obs_list)

# Select out my columns of interest
obs_clean <- obs %>% dplyr::select(c(Transect, ObsN, StartWind, RunType, FirstObs))
obs_clean <- obs_clean[!duplicated(obs_clean$Transect), ]  # Remove duplicated transects - some had shared observers, I'll just go with 1

# Re-make transect column, sometimes it gets messed up
summarize_df$Transect <- paste(summarize_df$RouteNumber, summarize_df$Year, sep=".")
dddf <- merge(summarize_df, obs_clean, by = "Transect", all.x = FALSE)   # Merge together

# Select years >= 2000
dddf <- dddf %>% filter(Year >= 2000)

write.csv(dddf, "~/space-time/data prep/base_100m_dataset_diversity.csv")


# -----------------------------#
#          FILTERING           | ------------------------------------------------------------------------------------------
# ----------------------------#

## Filter for the sites I want across the 27 comparison regions (pre-identified)

# Re-load in the base dataset generated above
dddf <- read.csv("~/space-time/data prep/base_100m_dataset_diversity.csv")

# Load in my whole diversity dataset which had all my regions of interest
filter <- read.csv("~/space-time/final datasets/archive_march/whole_dataset_richness_mar2021_version4.csv")

# Re-make the transect columns since they get messed up sometimes
dddf$Transect <- paste(dddf$RouteNumber, dddf$Year, sep=".")
filter$Transect <- paste(filter$RouteNumber, filter$Year, sep=".")

# Select the columns I want
filter <- filter %>% dplyr::select(ref, Transect, space.time, Forest.cover)

# Merge together by transect - this extracts
diversity <- merge(dddf, filter, by = "Transect")


reg <- read.csv("~/space-time/final datasets/SR2_mean_forest/richness_dataset_forest_FINAL.csv")
reg <- unique(reg$ref)
diversity <- diversity %>% filter(!ref == 4105) # Take out the spatial sites for region 4105, because the temporal site had no forested stops
diversity <- diversity %>% filter(ref %in% reg) # Remove <15 years of data, leaves me with 21
n_distinct(diversity$ObsN)
n_distinct(diversity$ref)

write.csv(diversity, "~/space-time/final datasets/evenness_dataset_total_FINAL.csv")
