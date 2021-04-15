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

# -------------------------------------------#
#   SPECIES RICHNESS ACROSS FORESTED STOPS   | ------------------------------------------------------------------------------------------
# ------------------------------------------#
# Load csv file back in
dd_long <- read.csv("check_dd_long.csv")



forestcodes <- read.csv("forestcodes.csv", header = T)
forestcodes <- forestcodes %>% dplyr::select(English_Common_Name, status) %>% distinct(English_Common_Name, status)
bbl <- read.csv("bbl_codes.csv") # had to make manual changes to YRWA, DEJU in Excel as well as add Sooty Grouse, Ruffed Grouse, Northern Bobwhite 
# since BBL doesn't provide codes for gallinaceous birds

ddf <- merge(dd_long, bbl, by = "English_Common_Name", all.x=FALSE)
ddf <- merge(ddf, forestcodes, by = "English_Common_Name")

# ddf2 <- ddf %>% filter(status == c("O", "FE")) ## if doing open birds at open stops

ddf2 <- ddf %>% filter(status == "F") ## if doing forest birds at forest stops


# Write to csv file
# write.csv(ddf2, "~/space-time/data prep/SR3_mean_open/dd_long_open_FINAL.csv") # if open
write.csv(ddf2, "dd_long_forest_FINAL.csv") # if forest


# Sum across the x stops that are forested >60% within 100m 
stopForest <- read.csv("~/arcmap/april 2021/forest_stops.csv")   ## change if looking at forested or open stops

stopForest$rte <- sub("\\.[0-9]+$", "", stopForest$X)
stopForest$RouteNumber <- sub("\\.[0-9]+$", "", stopForest$rte)
stopForest$Stop <- sub(".*\\.", "", stopForest$rte)



## IMPORTANT STEP!
# Because of renaming above under "clean up species", counts for the same species can appear twice but not in the same record
# Northern Flicker, Yellow-Rumped Warbler, and Dark-eyed Junco might have more than 1 record per stop

# Group by my columns down to the stop-level and summarize the counts 
#ddf <- read.csv("dd_long_open.csv")

ddf <- ddf2 %>% group_by(Transect, RouteNumber, Year, CountryNum, English_Common_Name, BBL, Stop) %>% summarize(Count = sum(Count))


df <- vector("list") # Initialize list

# Find list of my routes (all spatial and temporal) that were previously identified
rteno <- as.vector(unlist(read.csv("~/listofsites.txt", header = F)))


for( i in rteno ){
  # Filter forested stop dataset to focus on route i
  s <- stopForest %>% filter(RouteNumber == i )
  
  if(nrow(s) > 0){
    # For route i, keep only the stops that are forested (in stopForest)
    stop <- unique(s$Stop)
    f <- ddf %>% filter( RouteNumber == i ) %>% filter( Stop %in% stop )
    
    
    years <- unique(f$Year)
    
    # Initialize list
    f_list <- vector("list")
    

    for( y in years ){
      # For year y in the route, identify further which stops are forested
      syears <- s %>% filter(year == y )
      
      if(nrow(syears) > 0){
        
        
        stopyears <- unique(syears$Stop)
        
        # Keep only the stops, in route i year y, that are forested
        fyears <- f %>% filter( Year == y ) %>% filter( Stop %in% stopyears )
        
        # Calculate species richness at each stop within a site
        fyears <- fyears %>% group_by(Transect, RouteNumber, Year, CountryNum, Stop) %>% summarize(Richness = n_distinct(which(Count >= 1)))
        fyears$NumForestStops <- n_distinct(stopyears)
        
        f_list[[paste(y)]] <- fyears
        
      }else{
        # If year has no data in the bbs, just fill with dummy data
        Transect = NA
        RouteNumber = i
        Year = NA
        CountryNum = NA
        Stop = NA
        Richness = 0
        NumForestStops = 0
        
        f_list[[paste(y)]] <- data.frame(Transect, RouteNumber, Year, CountryNum, Stop, Richness, NumForestStops)
      }
    }

    f <- do.call("rbind", f_list) # Rbind into a big dataframe 
    
    # Paste the dataframe (for that route) into another initialized list
    df[[paste(i)]] <- f
    
  }else{
    # If the route has no forested stops, just fill with dummy data
    Transect = NA
    RouteNumber = i
    Year = NA
    CountryNum = NA
    Stop = NA
    Richness = 0
    NumForestStops = 0
    
    df[[paste(i)]] <- data.frame(Transect, RouteNumber, Year, CountryNum, Stop, Richness, NumForestStops)
  }
  
  
}

# Rbind list together into a dataframe
summarize_df <- do.call("rbind", df)

# Identify sites with no forested stops and remove them from the dataset
noforeststops <- summarize_df %>% filter(NumForestStops == 0) %>% distinct(RouteNumber)
summarize_df_clean <- summarize_df %>% filter(!NumForestStops == 0)


# Tabulate mean species richness by taking the average species richness across all forested stops (calculated above)
summarize_df <- summarize_df_clean %>% group_by(Transect, RouteNumber, Year, CountryNum, NumForestStops) %>% summarize(Richness_avg = mean(Richness))

write.csv(summarize_df, "~/space-time/data prep/SR2_mean_forest/summarize_df_forest_FINAL.csv")


# -----------------------------#
#    OBSERVERS & WEATHER       | ------------------------------------------------------------------------------------------
# ----------------------------#

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

write.csv(dddf, "~/space-time/data prep/SR2_mean_forest/base_100m_dataset_forest.csv")


# -----------------------------#
#          FILTERING           | ------------------------------------------------------------------------------------------
# ----------------------------#

## Filter for the sites I want across the 27 comparison regions (pre-identified)

# Re-load in the base dataset generated above
dddf <- read.csv("~/space-time/data prep/SR2_mean_forest/base_100m_dataset_forest.csv")

# Load in my whole richness dataset which had all my regions of interest
filter <- read.csv("~/space-time/final datasets/archive_march/whole_dataset_richness_mar2021_version4.csv")

# Re-make the transect columns since they get messed up sometimes
dddf$Transect <- paste(dddf$RouteNumber, dddf$Year, sep=".")
filter$Transect <- paste(filter$RouteNumber, filter$Year, sep=".")

# Select the columns I want
filter <- filter %>% dplyr::select(ref, Transect, space.time, Forest.cover)

# Merge together by transect - this extracts
richness <- merge(dddf, filter, by = "Transect")

# Check temporal summary
temporal <- richness %>% filter(space.time == 1)

# Take a look at how many years of data there are per temporal site
temporal_years <- temporal %>% group_by(RouteNumber) %>% summarize(nyears = n_distinct(Year))
below_15 <- temporal_years %>% filter(!nyears >= 15)  # Identify temporal sites with fewer than 15 years



richness <- richness %>% filter(!ref == 4105) # Take out the spatial sites for region 4105, because the temporal site had no forested stops
richness <- richness %>% filter(!ref %in% as.vector(unlist(below_15))) # Remove <15 years of data, leaves me with 21
n_distinct(richness$ObsN)

write.csv(richness, "~/space-time/final datasets/SR2_mean_forest/richness_dataset_forest_FINAL.csv")





### OPTIONAL & WIP - generate observer dataset if using an observer sub-model

dd_long <- read.csv("dd_long_forest.csv")
dat_obs <- dd_long %>% filter(ObsN %in% richness$ObsN)
n_distinct(dat_obs$ObsN)

rteno <- unique(dat_obs$RouteNumber)
df_obs <- vector("list")

for( i in rteno ){

       f <- dd_long %>% group_by(Transect, RouteNumber, Year, CountryNum, Stop) %>% summarize(Richness = n_distinct(which(Count >= 1)))
    
       df_obs[[paste(i)]] <- f
    
}

dat_obs <- do.call("rbind", df_obs)

dat_obs$Obs_ID <- as.integer(as.factor(dat_obs$ObsN))
dat_obs$Route_ID <- as.integer(as.factor(dat_obs$RouteNumber))
dat_obs$Eco_ID <- as.integer(as.factor(dat_obs$Ecoregion_L1Code))

dat_obs <- dat_obs[!is.na(dat_obs$Eco_ID),]
n_distinct(dat_obs$ObsN)
n_distinct(richness$ObsN)

richness_obs <- dat_obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo

write.csv(richness_obs, "~/space-time/final datasets/richness_observer_100m.csv")

