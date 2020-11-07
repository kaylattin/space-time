setwd("/Users/Kayla/Documents/BBS data")

library(tidyverse)

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

# Convert Route to a RouteNum that combines State/Province number and the individual Route within the state:
nrows <- length(d)

for(i in 1:nrows) {
  if(d$Route[i] < 100 & d$Route[i] > 10) {
    d$RouteNumber[i] <- paste(d$StateNum[i], d$Route[i], sep="0") 
  } 
  else if(d$Route[i] < 10) {
    d$RouteNumber[i] <- paste(d$StateNum[i], d$Route[i], sep="00")
  }
  else if(d$Route[i] > 100) {
    d$RouteNumber[i] <- paste(d$StateNum[i], d$Route[i], sep="")
  }
}

# Create unique transect column
d$Transect <- paste(d$RouteNumber, d$Year, sep=".")
d <- select(d, -c(Route, StateNum, RPID))
d <- select(d, -c(16:55)) # delete stops 12 - 

# Load in bird codes
names <- read.csv("BBSspecieslist.csv", header=T)
code <- read.csv("BBLcodes.csv", header=T)

# Load in list of forest species
forest <- read.csv("forestbirdcodes.csv", header=T)

# Match common names by AOU
d <- merge(d, names, by="AOU")

# Remove hybrids, unidentified observations, and subspecies & merge into 1 species for northern flicker, dark-eyed junco and yellow-rumped warbler
install.packages("anchors")
library(anchors)

d <- replace.value(d, "COMMONNAME", from = c("(Myrtle Warbler) Yellow-rumped Warbler", "(Audubon's Warbler) Yellow-rumped Warbler", "(unid. Myrtle/Audubon's) Yellow-rumped Warbler"),
                    to = "Yellow-rumped Warbler")
d <- replace.value(d, "COMMONNAME", from = c("(Yellow-shafted) Northern Flicker", "(Yellow-shafed Flicker) Northern Flicker", "(Red-shafted) Northern Flicker", "(unid. Red/Yellow Shafted) Northern Flicker"),
                   to = "Northern Flicker")
d <- replace.value(d, "COMMONNAME", from = c("(Slate-colored Junco) Dark-eyed Junco", "(Oregon Junco) Dark-eyed Junco", "(Pink-sided Junco) Dark-eyed Junco",
                                 "(White-winged Junco) Dark-eyed Junco", "(Gray-headed Junco) Dark-eyed Junco", "(unid. race) Dark-eyed Junco"),
                   to = "Dark-eyed Junco")

d <- d$COMMONNAME[d$COMMONNAME %in% c("African Collared Dove (a.k.a Ringed Turtle-Dove")] <- "African Collared Dove"
d <- d$COMMONNAME[d$COMMONNAME %in% c("(Great White Heron) Great Blue Heron")] <- "Great Blue Heron"
d <- d$COMMONNAME[d$COMMONNAME %in% c("(Harlan's Hawk) Red-tailed Hawk")] <- "Red-tailed Hawk"

d <- d %>% filter(!str_detect(COMMONNAME, 'hybrid'))
d <- d %>% filter(!str_detect(COMMONNAME, 'unid.'))

# write to csv to check if it worked
write.csv(d, "speciescheck.csv")

# Add in species codes and forest codes
d$SpeciesCode <- merge(code, d, by="COMMONNAME")
d$ForestDependent <- merge(forest, d, by="Species_code")

d <- d %>% relocate(SpeciesCode, .before = AOU) %>% head()
d <- d %>% relocate(ForestDependent, .after = AOU) %>% head()
d <- select(d, -c(Seq, ORDER, Family, Genus, Species)) %>% head()

# Select for only forest species
df <- d[which(d$Forest == 1),]

# Sum across the 11 stops and summarize so only 1 count per species per transect 
# deals with duplicate species produced by removal of hybrids above
df$Count<-df$Stop1+df$Stop2+df$Stop3+df$Stop4+df$Stop5+df$Stop6+df$Stop7+df$Stop8+df$Stop9+df$Stop10+df$Stop11
df %>% group_by(Transect, SpeciesCode) %>% summarize(Count = sum(Count))
df <- select(df, -c())

# Write Canada dataset
canada_dataset <- df[which(df$CountryNum == 124),]

# Remove years before 2000
d %>% filter(!str_detect(Year), c('1995, 1996, 1997, 1998, 1999'))

write.csv(canada_dataset,"canadaBBSdataset_R.csv")


## Adding in more info ----

# ecozone (obtained from ArcMap overlay)
ecozone <- read.csv("ecozoneBBS.csv", header=T)
df <- merge(df, ecozone, by = "Transect")

# % forest cover (obtained from extracting % mean forest cover from GFC forest layers in each transect)
forest <- read.csv()

# pivot long


df <- merge(df, forestcover, by = "Transect")

# RunType = 0 specification codes (obtained from NWRC)
run <-
df <- merge()

# Observer and weather info
obs <-
df <- merge()

d <- merge()

# % forest cover (obtained from extracting % mean forest cover from GFC forest layers in each transect)
forest <- 
  d <- merge()

# RunType = 0 specification codes (obtained from NWRC)
run <-
  d <- merge()

# Observer and weather info
obs <-
  d <- merge()

## Filter for my desired sites -----

# list of spatial sites:
# list of temporal sites:


write.csv()




## things I have to do manually:
# assign spatial sites to temporal sites
# get the lists for forest birds, BBL, forest cover, and ecozone
# remove species that don't appear in either dataset
# consolidate routes with >1 ecozone by referencing ArcMap
