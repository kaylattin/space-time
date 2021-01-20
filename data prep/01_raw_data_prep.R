setwd("/Users/kayla/Documents/thesis_data/bbs")

library(tidyverse)

sp <- read.csv("SpeciesList.csv")
sp <- sp %>% select(AOU, English_Common_Name)

forestcover <- read.csv("forestcover_master.csv", header=T, check.names = FALSE)

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



# -----------------------------#
#    US &CANADA FILTERING     | ------------------------------------------------------------------------------------------
# ----------------------------#


# Sum across the 11 stops and summarize so only 1 count per species per transect 
# deals with duplicate species produced by merging of subspecies & hybrids above
dd$Count<-dd$Stop1+dd$Stop2+dd$Stop3+dd$Stop4+dd$Stop5+dd$Stop6+dd$Stop7+dd$Stop8+dd$Stop9+dd$Stop10+dd$Stop11


# Select years >= 2000
df <- dd[which(dd$Year >= 2000),]

write.csv(df, "canada_us_bbs.csv")


# -------------------------#
#   ADDING IN MORE INFO   |-----------------------------------------------------------------------------------------------
# ------------------------#

#### FOREST COVER --------------------------------------------------------------------------------------------------------
# obtained from extracting % mean forest cover from GFC forest layers in each transect
forestcover <- read.csv("forestcover_master.csv", header=T, check.names = FALSE)

change <- forestcover %>% select(rte, change)
cover <- select(forestcover, -c(change))

# reformat to long
forest_long <- reshape(cover, v.names="Forest cover", varying = 2:21, timevar="Year", times=names(cover)[2:21],direction='long')
forest_long$Transect <- paste(forest_long$rte, forest_long$Year, sep=".")
forest_long <- select(forest_long, -c(rte, Year, id))

ddf <- merge(df, forest_long, by = "Transect")


### ECOREGIONS EPA LEVEL 1 obtained from overlay in ArcGis ---------------------------------------------------------------
ecoregions <- read.csv("routes_ecoregions.csv", header=T)-

ddf <- merge(ddf, ecoregions, by = "RouteNumber")




#### OBSERVER AND WEATHER  ---------------------------------------------------------------------
# Create unique placeholder ID for statenum + route
obs <- read.csv("weather.csv", header=T)

obs$placeholder <- paste(obs$StateNum, obs$Route, sep=".")

# Create another df to get a list of unique Route & StateNum's
r <- distinct(obs, StateNum, Route, .keep_all = TRUE)
r <- select(r, -c(5:21))
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


r <- select(r, -c(RouteDataID, StateNum, Route, RunType))
obs <- merge(obs, r, by = "placeholder", all.x = FALSE)
obs$Transect <- paste(obs$RouteNumber, obs$Year, sep=".")
obs_clean <- obs %>% filter(Year >= 2000) %>% select(c(Transect, ObsN, StartWind, RunType))
obs_clean <- obs_clean[!duplicated(obs_clean$Transect), ]

ddf <- merge(ddf, obs_clean, by = "Transect", all.x = FALSE)

write.csv(ddf, "clean_bbs_dataset.csv")

