setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)
# Load in list of coded forest species
forest <- read.csv("forestbirdcodes.csv", header=T)
# Load in BBS species list (originally .txt file from site) and BBL bird codes
names <- read.csv("speciesListBBS.csv", header=T)

code <- read.csv("BBLcodes.csv", header=T)

forestcover <- read.csv("FORESTCOVER_wide.csv", header=T, check.names = FALSE)

ecozone <- read.csv("AllTransects_Ecozones.csv", header=T)

obs <- read.csv("observerinfo_raw.csv", header=T)

run <- read.csv("RunType_NWRC.csv", header=T)





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

r <- select(r, -c(RouteDataID, Route, StateNum))
d <- inner_join(d, r, by = "placeholder")

# Create unique transect column
d$Transect <- paste(d$RouteNumber, d$Year, sep=".")
d <- select(d, -c(RPID, placeholder))
d <- select(d, -c(18:56)) # delete stops 12 to 50 

# Match common names by AOU
names <- select(names, -c(Seq, ORDER, Family, Genus, Species))
d <- inner_join(d, names, by="AOU")



write.csv(d, "prep_raw_data.csv")




# ------------------------#
#    CLEAN UP SPECIES     |
# ------------------------#


# d <- read.csv("prep_raw_data.csv")
# d$Transect <- paste(d$RouteNumber, d$Year, sep=".")

code <- select(code, -c(SP, CONF, SPEC6, CONF6))

library(anchors)

# Merge subspecies into 1 species for northern flicker, dark-eyed junco and yellow-rumped warbler
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
d <- replace.value(d, "COMMONNAME", from = c("(Black Brant"), to = "Brant")

detach(package:anchors, unload=TRUE) # detach b/c package masks select() function from dplyr and need it later

# Remove unid. observations and hybrids
## dd = 3607797 obs
dd <- d %>% filter(!str_detect(COMMONNAME, 'hybrid'))
dd <- d %>% filter(!str_detect(COMMONNAME, 'unid.'))

# Bring in BBL codes using Common Name
df <- merge(dd, code, by="COMMONNAME", all.x = TRUE)

# Merge info on species codes 0 == obligate, 1 == edge, 2 == shrub, 3 == not associated with forests
names(df)[names(df) == "SPEC"] <- "SpeciesCode"

write.csv(df, "progress.csv")




# save workspace, restart R, reload tidyverse
df <- read.csv("progress.csv")
df$Transect <- paste(df$RouteNumber, df$Year, sep=".")


names(forest)[names(forest) == "Code"] <- "SpeciesCode"
names(forest)[names(forest) == "Code.1"] <- "ForestCode"
names(forest)[names(forest) == "Forest.bird..0.no..1.yes."] <- "ForestDependent"
forest <- select(forest, -c(ForestDependent, Species))

df <- merge(df, forest, by = "SpeciesCode", all.x = TRUE)




# ------------------------#
#    CANADA FILTERING     |
# ------------------------#


# Sum across the 11 stops and summarize so only 1 count per species per transect 
# deals with duplicate species produced by merging of subspecies & hybrids above
df$Count<-df$Stop1+df$Stop2+df$Stop3+df$Stop4+df$Stop5+df$Stop6+df$Stop7+df$Stop8+df$Stop9+df$Stop10+df$Stop11
#summarize_df <- df %>% group_by(Transect, RouteNumber, Year, CountryNum, SpeciesCode, ForestCode) %>% summarize(Count = sum(Count))


# Select for only forest species 0 == forest obligate, 1 == edge; Canada; all years after 1999
df<- df[which(df$ForestCode == 0),]
df <- df[which(df$Year > 1999),]
df<- df[which(df$CountryNum == 124),]

library(plyr)
# put into wide to get the same species list for every single site
summarize_df <- ddply(df, c('Transect', 'SpeciesCode'), summarize, a = sum(Count))
summarize_df_n <- reshape(summarize_df, idvar="Transect", timevar="SpeciesCode", direction="wide")
summarize_df_n[is.na(summarize_df_n)] <- 0


summarize_df_n <- summarize_df_n %>% distinct(Transect, .keep_all = TRUE)




# save workspace & restart R (plyr won't work with tidyverse loaded in at the same time)
library(tidyverse)

# put back into long
ddf_wide <- inner_join(df, summarize_df_n, by = "Transect", all.x = TRUE)
ddf_wide2 <- ddf_wide %>% distinct(Transect, .keep_all = TRUE)

# select out columns I don't need
ddf_wide2 <- select(ddf_wide2, -c(X, AOU, SCINAME, COMMONNAME, ForestCode, Count, SpeciesCode, Stop1, Stop2, Stop3, Stop4, Stop5, Stop6, Stop7, Stop8, Stop9, Stop10, Stop11))


canada_df <- reshape(ddf_wide2, v.names = "Count",  varying = 8:134, timevar = "SpeciesCode", times = names(ddf_wide2)[8:134], direction = 'long')
canada_df <- select(canada_df, -id)

write.csv(canada_df,"canadaBBSdataset.csv")



# ------------------------#
#   ADDING IN MORE INFO   |
# ------------------------#


#### FOREST COVER -------------------------
# obtained from extracting % mean forest cover from GFC forest layers in each transect
change <- select(forestcover, c(RouteNumber, Change))
forestcover <- select(forestcover, -c(FID, Change))

# reformat to long
forest_long <- reshape(forestcover,v.names="Forest cover",varying = 2:20, timevar="Year",times=names(forestcover)[2:20],direction='long')
forest_long$Transect <- paste(forest_long$RouteNumber, forest_long$Year, sep=".")
forest_long <- select(forest_long, -c(RouteNumber, Year, id))
  
canada_df <- merge(canada_df, forest_long, by = "Transect")
canada_df <- merge(canada_df, change, by = "RouteNumber")



### ECOZONE (obtained from ArcMap overlay) --------------------------
ecozone <- select(ecozone, -c(cv2018_, AREA, PERIMETER))
canada_df <- merge(canada_df, ecozone, by = "RouteNumber")





#### OBSERVER AND WEATHER (downloaded from BBS site) -------------------------

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

r <- select(r, -c(RouteDataID, StateNum, Route, RunType))
obs <- merge(obs, r, by = "placeholder", all.x = FALSE)
obs$Transect <- paste(obs$RouteNumber, obs$Year, sep=".")
obs <- select(obs, c(Transect, ObsN, StateNum, StartWind, RunType))


canada_df <- merge(canada_df, obs, by = "Transect", all.x = FALSE)




# RUNTYPE = 0 specification codes (obtained from NWRC) ---------------------
names(run)[names(run) == "RouteNo"] <- "RouteNumber"
run$Transect <- paste(run$RouteNumber, run$Year, sep = ".")
run <- select(run, -c(RouteNumber, State, Route, Year))


canada_df <- merge(canada_df, run, by = "Transect", all.x = TRUE)


## Add province names for ease later
StateNum <- c(4, 11, 45, 56, 57, 65, 68, 75, 76, 79)
prov <- c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland & Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan")
provinces <- data.frame(StateNum, prov)

names(canada_df)[names(canada_df) == "StateNum.x"] <- "StateNum"

canada_df <- merge(canada_df, provinces, by = "StateNum", all.x = TRUE)



# total = 966978 observations
write.csv(canada_df, "complete_canada_dataset.csv")

