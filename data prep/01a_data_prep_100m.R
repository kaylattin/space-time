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

dd_long <- read.csv("check_dd_long.csv")

# Sum across the x stops that are forested >60% within 100m 
stopForest <- read.csv("~/arcmap/april 2021/forest_stops.csv")

stopForest$rte <- sub("\\.[0-9]+$", "", stopForest$X)
stopForest$RouteNumber <- sub("\\.[0-9]+$", "", stopForest$rte)
stopForest$Stop <- sub(".*\\.", "", stopForest$rte)

df <- vector("list")


rteno <- as.vector(unlist(read.csv("~/listofsites.txt", header = F)))

# for every one of my comparison regions, keep only the stops that are forested (in stopForest)
# and then sum counts for each species across those stops (after they've been filtered for)
for( i in rteno ){
  
  s <- stopForest %>% filter(RouteNumber == i )
  
  if(nrow(s) > 0){
  stop <- unique(s$Stop)
  f <- dd_long %>% filter( RouteNumber == i ) %>% filter( Stop %in% stop )


  years <- unique(f$Year)
  f_list <- vector("list")
  
  for( y in years ){
    syears <- s %>% filter(year == y )
    
    if(nrow(syears) > 0){
    stopyears <- unique(syears$Stop)
    
    fyears <- f %>% filter( Year == y ) %>% filter( Stop %in% stopyears )
    fyears <- fyears %>% group_by(Transect, RouteNumber, Year, CountryNum, English_Common_Name) %>% summarize(Count = sum(Count))
    fyears$NumForestStops <- n_distinct(stopyears)
    
    f_list[[paste(y)]] <- fyears
    
    }else{
      Transect = NA
      RouteNumber = i
      Year = NA
      CountryNum = NA
      English_Common_Name = NA
      Count = 0
      NumForestStops = 0
      
      f_list[[paste(y)]] <- data.frame(Transect, RouteNumber, Year, CountryNum, English_Common_Name, Count, NumForestStops)
    }
  }
  
  f <- do.call("rbind", f_list)
  
  df[[paste(i)]] <- f
  
  }else{
    Transect = NA
    RouteNumber = i
    Year = NA
    CountryNum = NA
    English_Common_Name = NA
    Count = 0
    NumForestStops = 0
    
    df[[paste(i)]] <- data.frame(Transect, RouteNumber, Year, CountryNum, English_Common_Name, Count, NumForestStops)
  }

  
}

summarize_df <- do.call("rbind", df)

noforeststops <- summarize_df %>% filter(NumForestStops == 0) %>% filter(Year > 2000) %>% distinct(RouteNumber)

summarize_df_clean <- summarize_df %>% filter(!NumForestStops == 0)

write.csv(summarize_df_clean, "summarize_df.csv")

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

write.csv(ddf, "base_100m_dataset.csv")


## filter for the sites I want across the 27 comparison regions and compute species richness!
ddf <- read.csv("~/space-time/data prep/base_100m_dataset.csv")
filter <- read.csv("~/space-time/final datasets/whole_dataset_richness_mar2021_version4.csv")

forestcodes <- read.csv("forestcodes.csv", header = T)
forestcodes <- forestcodes %>% dplyr::select(English_Common_Name, status_forest) %>% distinct(English_Common_Name, status_forest)
bbl <- read.csv("bbl_codes.csv") # had to make manual changes to YRWA, DEJU in Excel as well as add Sooty Grouse, Ruffed Grouse, Northern Bobwhite 
# since BBL doesn't provide codes for gallinaceous birds

ddff <- merge(ddf, bbl, by = "English_Common_Name", all.x=FALSE)
dddf <- merge(ddff, forestcodes, by = "English_Common_Name")

dddf <- dddf %>% filter(status_forest == "F")

dddf <- dddf %>% filter(!Count == 0)

filter$Transect <- paste(filter$RouteNumber, filter$Year, sep=".")
filter <- filter %>% dplyr::select(ref, Transect, space.time, Forest.cover)


final_df <- merge(dddf, filter, by = "Transect")

richness <- final_df %>% group_by(space.time, ref, Transect, RouteNumber, Year, ObsN, Forest.cover, FirstObs, NumForestStops) 

#%>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo


## remove any without forest stops
richness <- richness %>% filter(!NumForestStops == 0)

# any temporal years below 15 data points?
temporal <- richness %>% filter(space.time == 1)


temporal_years <- temporal %>% group_by(RouteNumber) %>% summarize(nyears = n_distinct(Year))
below_15 <- temporal_years %>% filter(!nyears >= 15)


# have to take out 4105
richness <- richness %>% filter(!ref == 4105)
richness <- richness %>% filter(!ref %in% as.vector(unlist(below_15)))
n_distinct(richness$ObsN)

write.csv(richness, "~/space-time/final datasets/richness_dataset_100m.csv")

## observer

dat_obs <- read.csv("clean_bbs_dataset_mar2021.csv")
dat_obs <- merge(dat_obs, bbl, by = "English_Common_Name", all.x = TRUE)
dat_obs <- dat_obs %>% filter(BBL %in% final_df$BBL) %>% filter(ObsN %in% richness$ObsN)
n_distinct(dat_obs$ObsN)
dat_obs$Obs_ID <- as.integer(as.factor(dat_obs$ObsN))
dat_obs$Route_ID <- as.integer(as.factor(dat_obs$RouteNumber))
dat_obs$Eco_ID <- as.integer(as.factor(dat_obs$Ecoregion_L1Code))

dat_obs <- dat_obs[!is.na(dat_obs$Eco_ID),]
n_distinct(dat_obs$ObsN)
n_distinct(richness$ObsN)

richness_obs <- dat_obs %>% group_by(RouteNumber, Year, ObsN, Eco_ID, Obs_ID, Route_ID ) %>%
  summarise(Richness = n_distinct(which(Count >= 1))) # how many species present in that route / year combo

write.csv(richness_obs, "~/space-time/final datasets/richness_observer_100m.csv")

