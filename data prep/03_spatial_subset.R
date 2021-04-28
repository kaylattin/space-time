library(tidyverse)

sub <- read.csv("~/space-time/data prep/subsetting_spatial_comma_forest.csv")
sub$Transect <- paste(sub$RouteNumber, sub$Year, sub$ref, sep=".")

t <- read.csv("~/space-time/final datasets/SR1_total/total_richness_FINAL.csv")
t$Transect <- paste(t$RouteNumber, t$Year, t$ref, sep=".")


f <- read.csv("~/space-time/final datasets/SR2_mean_forest/richness_forest_FINAL.csv")
f$Transect <- paste(f$RouteNumber, f$Year, f$ref, sep=".")


o <- read.csv("~/space-time/final datasets/SR3_mean_open/richness_open_FINAL.csv")
o$Transect <- paste(o$RouteNumber, o$Year, o$ref, sep=".")


subSub <- sub %>% filter(Include == "x")

sublist <- unique(subSub$Transect)

# total richness
tsub <- t %>% filter(Transect %in% sublist)
n_distinct(tsub$ref)

# forest
fsub <- f %>% filter(Transect %in% sublist)
n_distinct(fsub$ref)

# open
osub <- o %>% filter(Transect %in% sublist)
n_distinct(osub$ref)

write.csv(tsub, "~/space-time/final datasets/SR1_total/total_richness_FINALsubset.csv")
write.csv(fsub, "~/space-time/final datasets/SR2_mean_forest/richness_forest_FINALsubset.csv")
write.csv(osub, "~/space-time/final datasets/SR3_mean_open/richness_open_FINALsubset.csv")