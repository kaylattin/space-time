setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)

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

# Create another df to get a list of unique Route & StateNum's
r <- distinct(d, StateNum, Route, .keep_all = TRUE)
r <- select(r, -c(5:57))
r <- select(r, -2)

# Convert Route to a RouteNum that combines State/Province number and the individual Route within the state:
nrows <- length(r$Route)

pb <- winProgressBar(title="progress", min=0, max=nrows, width=300)

for(i in 1:nrows) {
  if(r$Route[i] < 100 & r$Route[i] > 10) {
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
d <- merge(d, r, by = "RouteDataID")

# Create unique transect column
d$Transect <- paste(d$RouteNumber, d$Year, sep=".")
d <- select(d, -c(StateNum.x, StateNum.y, Route.y, RPID))
d <- select(d, -c(StateNum, RPID))
d <- select(d, -c(17:55)) # delete stops 12 to 50 

# Load in bird codes
names <- read.csv("BBSspecieslist.csv", header=T)
code <- read.csv("BBLcodes.csv", header=T)

# Load in list of forest species
forest <- read.csv("forestbirdcodes.csv", header=T)

# Match common names by AOU
d <- merge(d, names, by="AOU")

# Merge subspecies into 1 species for northern flicker, dark-eyed junco and yellow-rumped warbler
library(anchors)
d <- replace.value(d, "COMMONNAME", from = c("(Myrtle Warbler) Yellow-rumped Warbler", "(Audubon's Warbler) Yellow-rumped Warbler", "(unid. Myrtle/Audubon's) Yellow-rumped Warbler"),
                    to = "Yellow-rumped Warbler")
d <- replace.value(d, "COMMONNAME", from = c("(Yellow-shafted) Northern Flicker", "(Yellow-shafted Flicker) Northern Flicker", "(Red-shafted) Northern Flicker", "(unid. Red/Yellow Shafted) Northern Flicker"),
                   to = "Northern Flicker")
d <- replace.value(d, "COMMONNAME", from = c("(Slate-colored Junco) Dark-eyed Junco", "(Oregon Junco) Dark-eyed Junco", "(Pink-sided Junco) Dark-eyed Junco",
                                 "(White-winged Junco) Dark-eyed Junco", "(Gray-headed Junco) Dark-eyed Junco", "(unid. race) Dark-eyed Junco"),
                   to = "Dark-eyed Junco")

d <- replace.value(d, "COMMONNAME", from = c("African Collared Dove (a.k.a Ringed Turtle-Dove"),
                   to = "African Collared Dove")
d <- replace.value(d, "COMMONNAME", from = c("(Great White Heron) Great Blue Heron"), to = "Great Blue Heron")
d <- replace.value(d, "COMMONNAME", from = c("(Harlan's Hawk) Red-tailed Hawk"), to = "Red-tailed Hawk")


detach(package:anchors, unload=TRUE) # detach b/c package masks select() function from dplyr and need it later

# Bring in BBL codes
d <- merge(d, code, by="COMMONNAME")

# Remove unid. observations and hybrids
d <- d %>% filter(!str_detect(COMMONNAME, 'hybrid'))
d <- d %>% filter(!str_detect(COMMONNAME, 'unid.'))

# ---------------------------------------------------

### At this point, need to save workspace & restart R, reloading only the tidyverse pckg ----
# Rearrange the data frame columns
library(tidyverse)
df$Count<-df$Stop1+df$Stop2+df$Stop3+df$Stop4+df$Stop5+df$Stop6+df$Stop7+df$Stop8+df$Stop9+df$Stop10+df$Stop11

# Sum across the 11 stops and summarize so only 1 count per species per transect 
# deals with duplicate species produced by removal of hybrids above
df$RouteNumber <- as.numeric(df$RouteNumber)
summarize_df <- df %>% group_by(Transect, RouteNumber, Year, SpeciesCode) %>% summarize(Count = sum(Count))

# Bring information back in
# Merge info on species codes and forest-dependent codes (0 == not forest dependent, 1 == forest dependent)

names(d)[names(d) == "SPEC"] <- "SpeciesCode"
names(code)[names(code) == "SPEC"] <- "SpeciesCode"
names(forest)[names(forest) == "Code"] <- "SpeciesCode"
names(forest)[names(forest) == "Forest.bird..0.no..1.yes."] <- "ForestDependent" 
names(forest)[names(forest) == "Code.1"] <- "ForestCode"


ddf <- merge(summarize, code, by = "SpeciesCode")
ddf <- merge(ddf, forest, by = "SpeciesCode")
ddf <- select(ddf, -c(SP, CONF, SPEC6, CONF6, COMMONNAME))


# Select for only forest species
ddf <- ddf[which(ddf$ForestDependent == 1),]

# Remove years before 2000
ddf %>% filter(!str_detect(Year), c('1995, 1996, 1997, 1998, 1999'))


# Write Canada dataset
canada_df <- df_full[which(df_full$CountryNum == 124),]

write.csv(canada_dataset,"canadaBBSdataset_R.csv")


## Adding in more info ---------------

# ecozone (obtained from ArcMap overlay)
ecozone <- read.csv("ecozoneBBS.csv", header=T)
df <- merge(df, ecozone, by = "Transect")

# % forest cover (obtained from extracting % mean forest cover from GFC forest layers in each transect)

forestcover <- read.csv("forestcover.csv", header=T) # this is the first sheet in the database
names(forestcover)

forestcover <- forest[, -21:-30]

forest <- read.csv("forestcover.csv", header=T) # this is the first sheet in the database
names(forest)

forest <- forest[, -21:-30]

# reformat to long
# I would like a new variable, v.names="Forest cover", with the % estimates, which I get
# by running through columns 2 to 20 (varying=2:20); I know which estimate I am reading by looking
# at the column names (times=names(data)[2:20]), and capture estimate column names
# in a new variable (timevar="Year")
forest_long <- reshape(data,v.names="Forest cover",varying = 2:20,timevar="Year",times=names(data)[2:20],direction='long')


df <- merge(df, forest_long, by = "Transect")

# RunType = 0 specification codes (obtained from NWRC)
run <-
df <- merge()

# Observer and weather info
obs <-
df <- merge()

d <- merge()

## Filter for my desired sites -----

# list of spatial sites:
# list of temporal sites:


write.csv()




## things I have to do manually:
# assign spatial sites to temporal sites
# filtering out spatial sites without data in 2018
# filtering out temporal sites with <5 years of data
# get the lists for forest birds, BBL, forest cover, and ecozone
# remove species that don't appear in either dataset - i can probably filter these out here, but the id process is in excel
# consolidate routes with >1 ecozone by referencing ArcMap
