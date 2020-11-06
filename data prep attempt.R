setwd("/Users/Kayla/Documents/BBS data")

install.packages("tidyverse")
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

# Create unique transect column
d$Transect <- paste(d$Route,".",d$Year)



# Load in bird codes
code <- 
  
# Load in list of forest species
forest <-
  

# Remove all "unid." species
d %>% filter(!str_detect(Species, 'unid.'))

# Remove hybrids and subspecies & merge into 1 species
# dark-eyed junco and yellow-throated warbler

d$Species[d$Species %in% c("(Myrtle Warbler) Yellow-Throated Warbler", "(Audubon Warbler) Yellow-throated Warbler")] <- "Yellow-throated Warbler"


# Add in species codes
d$Species_code <- merge(code, d, by="Transect")


# Remove years before 2000

d %>% filter()


# Sum counts from stops 1 - 11 and discard the rest
d$Count <- rowSums(a[,7:17])

# Remove unnecessary columns
d <- select(d, -c(RPID))
d <- select(d, -c(8:50))


# Write Canada dataset
canada_dataset <- d[which(full_dataset$CountryNum == 124),]

write.csv(canada_dataset,"canada_BBS_dataset.csv")


## Adding in more info ----

# ecozone (obtained from ArcMap overlay)
ecozone <-
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
