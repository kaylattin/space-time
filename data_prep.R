setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)


# ------------------------#
#      PREP RAW DATA      |
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

pb <- winProgressBar(title="progress", min=0, max=nrows, width=300)

for(i in 1:nrows) {
  if(r$Route[i] < 100 & r$Route[i] >= 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="0")
} 
  else if(r$Route[i] < 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="00")
  }
  else {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="")
  }
  Sys.sleep(0.1); setWinProgressBar(pb,i,title=paste("Row:", i, "out of", nrows, "done"))
}
close(pb)
d <- merge(d, r, by = "placeholder")

# Create unique transect column
d$Transect <- paste(d$RouteNumber, d$Year, sep=".")
d <- select(d, -c(StateNum.x, StateNum.y, Route.y, RPID, placeholder))
d <- select(d, -c(17:55)) # delete stops 12 to 50 

# Load in BBS species list (originally .txt file from site) and BBL bird codes
names <- read.csv("speciesListBBS.csv", header=T)

# Match common names by AOU
d <- merge(d, names, by="AOU")

write.csv(d, "prep_raw_data.csv")

# ------------------------#
#    CLEAN UP SPECIES     |
# ------------------------#

d <- read.csv("prep_raw_data.csv")
code <- read.csv("BBLcodes_withAOU.csv", header=T)

# Merge subspecies into 1 species for northern flicker, dark-eyed junco and yellow-rumped warbler
library(anchors)
d <- replace.value(d, "COMMONNAME", from = c("(Myrtle Warbler) Yellow-rumped Warbler", "(Audubon's Warbler) Yellow-rumped Warbler", "(unid. Myrtle/Audubon's) Yellow-rumped Warbler"),
                    to = "Yellow-rumped Warbler")
d <- replace.value(d, "COMMONNAME", from = c("(Yellow-shafted) Northern Flicker", "(Yellow-shafted Flicker) Northern Flicker", "(Red-shafted) Northern Flicker", "(unid. Red/Yellow Shafted) Northern Flicker"),
                   to = "Northern Flicker")
d <- replace.value(d, "COMMONNAME", from = c("(Slate-colored Junco) Dark-eyed Junco", "(Oregon Junco) Dark-eyed Junco", "(Pink-sided Junco) Dark-eyed Junco",
                                 "(White-winged Junco) Dark-eyed Junco", "(Gray-headed Junco) Dark-eyed Junco", "(unid. race) Dark-eyed Junco"),
                   to = "Dark-eyed Junco")

## others that aren't forest birds but for the sake of consistency
d <- replace.value(d, "COMMONNAME", from = c("African Collared Dove (a.k.a Ringed Turtle-Dove"),
                   to = "African Collared Dove")
d <- replace.value(d, "COMMONNAME", from = c("(Great White Heron) Great Blue Heron"), to = "Great Blue Heron")
d <- replace.value(d, "COMMONNAME", from = c("(Harlan's Hawk) Red-tailed Hawk"), to = "Red-tailed Hawk")

detach(package:anchors, unload=TRUE) # detach b/c package masks select() function from dplyr and need it later

# Remove unid. observations and hybrids
## dd = 3607797 obs
dd <- d %>% filter(!str_detect(COMMONNAME, 'hybrid'))
dd <- d %>% filter(!str_detect(COMMONNAME, 'unid.'))

# Bring in BBL codes using AOU
df <- merge(dd, code, by="AOU", all.x = TRUE)

# Merge info on species codes 0 == obligate, 1 == edge, 2 == shrub, 3 == not associated with forests
# Load in list of coded forest species
forest <- read.csv("forestbirdcodes.csv", header=T)

names(df)[names(df) == "SPEC"] <- "SpeciesCode"
names(code)[names(code) == "SPEC"] <- "SpeciesCode"
names(forest)[names(forest) == "Code"] <- "SpeciesCode"
names(forest)[names(forest) == "Forest.bird..0.no..1.yes."] <- "ForestDependent" 
names(forest)[names(forest) == "Code.1"] <- "ForestCode"

df <- merge(df, forest, by = "SpeciesCode", all.x = TRUE)

write.csv(df, "clean_up_species.csv")

# ------------------------#
#    CANADA FILTERING     |
# ------------------------#

# Sum across the 11 stops and summarize so only 1 count per species per transect 
# deals with duplicate species produced by removal of hybrids above
df$Count<-df$Stop1+df$Stop2+df$Stop3+df$Stop4+df$Stop5+df$Stop6+df$Stop7+df$Stop8+df$Stop9+df$Stop10+df$Stop11
df$RouteNumber <- as.numeric(df$RouteNumber)
summarize_df <- df %>% group_by(Transect, RouteNumber, Year, CountryNum, SpeciesCode, ForestCode) %>% summarize(Count = sum(Count))

# Select for only forest species 0 == forest obligate, 1 == edge
ddf0 <- summarize_df[which(summarize_df$ForestCode == 0),]
ddf1 <- summarize_df[which(summarize_df$ForestCode == 1),]
ddf <- rbind(ddf0, ddf1)

# Remove years before 2000
ddf <- ddf[which(ddf$Year > 1999),]
ddf <- ddf[order(ddf$RouteNumber),]

# Write Canada dataset
canada_df <- ddf[which(ddf$CountryNum == 124),]

write.csv(canada_df,"canadaBBSdataset_R.csv")


# ------------------------#
#   ADDING IN MORE INFO   |
# ------------------------#

# close R to detach libraries and run below
setwd("/Users/Kayla/Documents/BBS data")
library(tidyverse)
canada_df <- read.csv("canadaBBSdataset_R.csv")
#### FOREST COVER -------------------------
# obtained from extracting % mean forest cover from GFC forest layers in each transect
forestcover <- read.csv("FORESTCOVER_wide.csv", header=T, check.names = FALSE)
change <- select(forestcover, c(RouteNumber, Change))
forestcover <- select(forestcover, -Change)

# reformat to long
# I would like a new variable, v.names="Forest cover", with the % estimates, which I get
# by running through columns 2 to 20 (varying=3:21); I know which estimate I am reading by looking
# at the column names (times=names(data)[2:20]), and capture estimate column names
# in a new variable (timevar="Year")
newforest <- reshape(forestcover,v.names="Forest cover",varying = 3:21, timevar="Year",times=names(forestcover)[3:21],direction='long')
newforest$Transect <- paste(newforest$RouteNumber, newforest$Year, sep=".")
canada_df <- merge(canada_df, newforest, by = "Transect")
names(canada_df)[names(canada_df) == "RouteNumber.x"] <- "RouteNumber"
canada_df <- merge(canada_df, change, by = "RouteNumber")

### ECOZONE (obtained from ArcMap overlay) --------------------------
ecozone <- read.csv("AllTransects_Ecozones.csv", header=T)
ecozone <- select(ecozone, -c(cv2018_, AREA, PERIMETER))
canada_df <- merge(canada_df, ecozone, by = "RouteNumber", no.dups = TRUE)


#### OBSERVER AND WEATHER (downloaded from BBS site) -------------------------
obs <- read.csv("observerinfo_raw.csv", header=T)

# Create unique placeholder ID for statenum + route
obs$placeholder <- paste(obs$StateNum, obs$Route, sep=".")

# Create another df to get a list of unique Route & StateNum's
r <- distinct(obs, StateNum, Route, .keep_all = TRUE)
r <- select(r, -c(5:21))
r <- select(r, -2)

# Convert Route to a RouteNum that combines State/Province number and the individual Route within the state
# i.e. Statenum = 04 and Route = 001 becomes RouteNumber = 4001
# StateNum goes up to 2 digits and Route goes up to 3 digits
nrows <- length(r$Route)

pb <- winProgressBar(title="progress", min=0, max=nrows, width=300)

for(i in 1:nrows) {
  if(r$Route[i] < 100 & r$Route[i] >= 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="0")
  } 
  else if(r$Route[i] < 10) {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="00")
  }
  else {
    r$RouteNumber[i] <- paste(r$StateNum[i], r$Route[i], sep="")
  }
  Sys.sleep(0.1); setWinProgressBar(pb,i,title=paste("Row:", i, "out of", nrows, "done"))
}
close(pb)
obs <- merge(obs, r, by = "placeholder", all.x = FALSE)
obs$Transect <- paste(obs$RouteNumber, obs$Year, sep=".")
obs <- select(obs, c(Transect, ObsN, RouteNumber, Year, StartWind, RunType.x))

# Remove years before 2000
obs <- obs[which(obs$Year > 1999),]
obs <- obs[order(obs$RouteNumber),]

canada_df <- select(canada_df, -RouteNumber.y)
canada_df_T <- merge(canada_df, obs, by = "Transect", all.x = FALSE)
canada_df_T <- select(canada_df_T, -c(Year.x, Year.y))

# RUNTYPE = 0 specification codes (obtained from NWRC) ---------------------
run <- read.csv("RunType_NWRC.csv", header=T)
names(run)[names(run) == "RouteNo"] <- "RouteNumber"
run$Transect <- paste(run$RouteNumber, run$Year, sep = ".")
canada_df_T <- merge(canada_df_T, run, by = "Transect", all.x = TRUE)

canada_df_T <- select(canada_df_T, -c(Year.y, X, FID, id, RouteNumber, RouteNumber.y, State, Route))
names(canada_df_T)[names(canada_df_T) == "RouteNumber.x"] <- "RouteNumber"
names(canada_df_T)[names(canada_df_T) == "RunType.x"] <- "RunType"
names(canada_df_T)[names(canada_df_T) == "Year.x"] <- "Year"

write.csv(canada_df_T, "complete_canada_dataset.csv")


## next steps:
# identify sites >20% change from 2000 to 2018 = temporal sites
# from these sites, identify those that are >5 years of data

# filtering out spatial sites without data in 2018
# for each temporal site, identify list of spatial sites that cover same gradient of % cover in the same ecozone
# create 2 datasets - one temporal and one spatial


## things I still have to do manually:
# get the lists for forest birds, BBL codes, forest cover from ArcGIS, ecozone from ArcMap, runtype from NWRC
# remove species that don't appear in either dataset - I can probably filter these out here, but the id process is in excel
#### or I can use aggregate to identify species with count = 0 across all routes in each temporal and spatial dataset


## final step:
# see if my output matches with the one I created half-manually in Excel w/o code documentation
