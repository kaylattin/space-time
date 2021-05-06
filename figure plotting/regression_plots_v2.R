## load up stuff
setwd("/Users/kayla/Documents/space-time/final datasets")
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)
library(scales)
library(rethinking)

rm(list = ls())
gc()


########## TOTAL ABUNDANCE ###############

# d <- read.csv("~/space-time/final datasets/TA1_total/tot_abundance_FINAL.csv")
# d$Region <- as.integer(as.factor(d$ref))
# d$Obs_ID <- as.integer(as.factor(d$ObsN))
# d$Cover_std <- as.vector(scale(d$Forest.cover))
# 
# load("~/space-time/final datasets/TA1_total/ta_total_apr29.RData")
# draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))
# draws 
# 
# pdf("ta_total_regression2.pdf", height = 10)
# 
# par(mfrow=c(3,2),
#     omi=c(0.3, 0.3, 0.3, 0.3))
# 
# for(i in 1:33){
#   d_filter <- d %>% filter(Region == i)
#   d_filter$Colour= "#2c7bb6"
#   d_filter$Shape = 16
#   # Set new column values to appropriate colours
#   d_filter$Colour[d_filter$space.time == 2] = "#ED432D"
#   d_filter$Shape[d_filter$space.time == 2] = 17
#   
#   d_space <- d_filter %>% filter(space.time == 2)
#   d_time <- d_filter %>% filter(space.time == 1)
#   n <- unique(d_space$Obs_ID)
#   n2 <- unique(d_time$Obs_ID)
#   
#   x.seq <- seq( from=min(d_filter$Cover_std), to = max(d_filter$Cover_std), length.out = 30 )
#   
#   
#   mu.space <- sapply(x.seq,  function(x) exp( mean( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) )
#   mu.time <- sapply(x.seq,  function(x) exp( mean( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) )
#   
#   ci.space <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) ) 
#   ci.time <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) )
#   
#   
#   plot(d_filter$Cover_std, d_filter$TA, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
#        xaxt = "n", yaxt = "n", cex.lab = 1.25, cex.main = 1.5, 
#        main = paste("Region", i), xlab = "Percent forest cover", ylab = "Total abundance")
#   
#   axis(1, at = c(min(d_filter$Cover_std), median(d_filter$Cover_std), max(d_filter$Cover_std)), 
#        labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
#        cex.axis = 1.25)
#   axis(2, cex.axis = 1.25)
#   line <- d %>% filter(Region == 3)
#   abline(v = median(line$Cover_std), lty = 2, col = alpha("black", 0.2))
#   
#   shade( ci.space, x.seq, col = alpha("#ED432D", 0.05))
#   shade( ci.time, x.seq, col = alpha("#2c7bb6", 0.05))
#   
#   
#   lines( x.seq, mu.time, col = "#2c7bb6", lwd = 1.5)
#   lines( x.seq, mu.space, col = "#ED432D", lwd = 1.5 )
#   
# }
# dev.off()


########## MEAN ABUNDANCES - FOREST ##########

d <- read.csv("~/space-time/final datasets/TA2_mean_forest/ta_forest_FINALall.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

r <- read.csv("~/space-time/data prep/remove_from_25004.csv")
r$id <- paste(r$remove, r$RouteNumber, sep=".")
r <- unique(r$id)

d$id <- paste(d$ref, d$RouteNumber, sep=".")
d <- d %>% filter(!id %in% r)



load("~/space-time/final datasets/TA2_mean_forest/ta_forest_all.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))


pdf("ta_forest_regression_all.pdf", height = 10)

par(mfrow=c(3,2),
    omi=c(0.3, 0.3, 0.3, 0.3))

for(i in 1:33){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#2c7bb6"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#ED432D"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  n <- unique(d_space$Obs_ID)
  n2 <- unique(d_time$Obs_ID)
  
  x.seq <- seq( from=min(d_filter$Cover_std), to = max(d_filter$Cover_std), length.out = 30 )
  
  
  mu.space <- sapply(x.seq,  function(x) mean( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) 
  mu.time <- sapply(x.seq,  function(x) mean( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) 
  
  ci.space <- sapply(x.seq,  function(x) PI( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) 
  ci.time <- sapply(x.seq,  function(x) PI( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) 
  
  
  plot(d_filter$Cover_std, d_filter$TA_avg, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.25, cex.main = 1.5, 
       main = paste("Region", i), xlab = "Percent forest cover", ylab = "Mean abundance per stop")
  
  axis(1, at = c(min(d_filter$Cover_std), median(d_filter$Cover_std), max(d_filter$Cover_std)), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.25)
  axis(2, cex.axis = 1.25)
  line <- d %>% filter(Region == 12)
  abline(v = median(line$Cover_std), lty = 2, col = alpha("black", 0.2))
  
  shade( ci.space, x.seq, col = alpha("#ED432D", 0.05))
  shade( ci.time, x.seq, col = alpha("#2c7bb6", 0.05))
  
  
  lines( x.seq, mu.time, col = "#2c7bb6", lwd = 1.5)
  lines( x.seq, mu.space, col = "#ED432D", lwd = 1.5 )
  
}
dev.off()


####### MEAN ABUNDANCES - OPEN ############

d <- read.csv("~/space-time/final datasets/TA3_mean_open/ta_open_FINALall.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

r <- read.csv("~/space-time/data prep/remove_from_25004.csv")
r$id <- paste(r$remove, r$RouteNumber, sep=".")
r <- unique(r$id)

d$id <- paste(d$ref, d$RouteNumber, sep=".")
d <- d %>% filter(!id %in% r)


load("~/space-time/final datasets/TA3_mean_open/ta_open_all.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))


pdf("ta_open_regression_all.pdf", height = 10)

par(mfrow=c(3,2),
    omi=c(0.3, 0.3, 0.3, 0.3))

for(i in 1:33){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#2c7bb6"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#ED432D"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  n <- unique(d_space$Obs_ID)
  n2 <- unique(d_time$Obs_ID)
  
  x.seq <- seq( from=min(d_filter$Cover_std), to = max(d_filter$Cover_std), length.out = 30 )
  
  
  mu.space <- sapply(x.seq,  function(x) mean( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) 
  mu.time <- sapply(x.seq,  function(x) mean( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) 
  
  ci.space <- sapply(x.seq,  function(x) PI( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) 
  ci.time <- sapply(x.seq,  function(x) PI( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) 
  
  
  plot(d_filter$Cover_std, d_filter$TA_avg, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.25, cex.main = 1.5, 
       main = paste("Region", i), xlab = "Percent forest cover", ylab = "Mean abundance per stop")
  
  axis(1, at = c(min(d_filter$Cover_std), median(d_filter$Cover_std), max(d_filter$Cover_std)), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.25)
  axis(2, cex.axis = 1.25)
  line <- d %>% filter(Region == 3)
  abline(v = median(line$Cover_std), lty = 2, col = alpha("black", 0.2))
  
  shade( ci.space, x.seq, col = alpha("#ED432D", 0.05))
  shade( ci.time, x.seq, col = alpha("#2c7bb6", 0.05))
  
  
  lines( x.seq, mu.time, col = "#2c7bb6", lwd = 1.5)
  lines( x.seq, mu.space, col = "#ED432D", lwd = 1.5 )
  
}
dev.off()





####### TOTAL SPECIES RICHNESS ##############

# d <- read.csv("~/space-time/final datasets/SR1_total/total_richness_FINALsubset.csv")
# d$Region <- as.integer(as.factor(d$ref))
# d$Obs_ID <- as.integer(as.factor(d$ObsN))
# d$Cover_std <- as.vector(scale(d$Forest.cover))
# 
# load("~/space-time/final datasets/SR1_total/total_richness_apr29.RData")
# draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))
# 
# pdf("total_richness_regression2.pdf", height = 10)
# 
# par(mfrow=c(3,2),
#     omi=c(0.3, 0.3, 0.3, 0.3))
# 
# for(i in 1:33){
#   d_filter <- d %>% filter(Region == i)
#   d_filter$Colour= "#2c7bb6"
#   d_filter$Shape = 16
#   # Set new column values to appropriate colours
#   d_filter$Colour[d_filter$space.time == 2] = "#ED432D"
#   d_filter$Shape[d_filter$space.time == 2] = 17
#   
#   d_space <- d_filter %>% filter(space.time == 2)
#   d_time <- d_filter %>% filter(space.time == 1)
#   n <- unique(d_space$Obs_ID)
#   n2 <- unique(d_time$Obs_ID)
#   
#   x.seq <- seq( from=min(d_filter$Cover_std), to = max(d_filter$Cover_std), length.out = 30 )
#   
#   
#   mu.space <- sapply(x.seq,  function(x) exp( mean( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) )
#   mu.time <- sapply(x.seq,  function(x) exp( mean( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) )
#   
#   ci.space <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) ) 
#   ci.time <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) )
#   
#   
#   plot(d_filter$Cover_std, d_filter$Richness, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
#        xaxt = "n", yaxt = "n", cex.lab = 1.25, cex.main = 1.5, 
#        main = paste("Region", i), xlab = "Percent forest cover", ylab = "Total abundance")
#   
#   axis(1, at = c(min(d_filter$Cover_std), median(d_filter$Cover_std), max(d_filter$Cover_std)), 
#        labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
#        cex.axis = 1.25)
#   axis(2, cex.axis = 1.25)
#   line <- d %>% filter(Region == 3)
#   abline(v = median(line$Cover_std), lty = 2, col = alpha("black", 0.2))
#   
#   shade( ci.space, x.seq, col = alpha("#ED432D", 0.05))
#   shade( ci.time, x.seq, col = alpha("#2c7bb6", 0.05))
#   
#   
#   lines( x.seq, mu.time, col = "#2c7bb6", lwd = 1.5)
#   lines( x.seq, mu.space, col = "#ED432D", lwd = 1.5 )
#   
# }
# dev.off()




#### MEAN RICHNESS - FOREST #######

d <- read.csv("~/space-time/final datasets/SR2_mean_forest/richness_forest_FINALall.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

r <- read.csv("~/space-time/data prep/remove_from_25004.csv")
r$id <- paste(r$remove, r$RouteNumber, sep=".")
r <- unique(r$id)

d$id <- paste(d$ref, d$RouteNumber, sep=".")
d <- d %>% filter(!id %in% r)



load("~/space-time/final datasets/SR2_mean_forest/richness_forest_all.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))

pdf("richness_forest_regression_all.pdf", height = 10)

par(mfrow=c(3,2),
    omi=c(0.3, 0.3, 0.3, 0.3))


for(i in 1:33){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#2c7bb6"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#ED432D"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  n <- unique(d_space$Obs_ID)
  n2 <- unique(d_time$Obs_ID)
  
  x.seq <- seq( from=min(d_filter$Cover_std), to = max(d_filter$Cover_std), length.out = 30 )
  
  
  mu.space <- sapply(x.seq,  function(x) mean( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) 
  mu.time <- sapply(x.seq,  function(x) mean( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) 
  
  ci.space <- sapply(x.seq,  function(x) PI( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) 
  ci.time <- sapply(x.seq,  function(x) PI( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) 
  
  
  plot(d_filter$Cover_std, d_filter$Richness_avg, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.25, cex.main = 1.5, 
       main = paste("Region", i), xlab = "Percent forest cover", ylab = "Mean species richness")
  
  axis(1, at = c(min(d_filter$Cover_std), median(d_filter$Cover_std), max(d_filter$Cover_std)), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.25)
  axis(2, cex.axis = 1.25)
  line <- d %>% filter(Region == 11)
  abline(v = median(line$Cover_std), lty = 2, col = alpha("black", 0.2))
  
  shade( ci.space, x.seq, col = alpha("#ED432D", 0.05))
  shade( ci.time, x.seq, col = alpha("#2c7bb6", 0.05))
  
  
  lines( x.seq, mu.time, col = "#2c7bb6", lwd = 1.5)
  lines( x.seq, mu.space, col = "#ED432D", lwd = 1.5 )
  
}
dev.off()




#### MEAN RICHNESS - OPEN ######
d <- read.csv("~/space-time/final datasets/SR3_mean_open/richness_open_FINALall.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

r <- read.csv("~/space-time/data prep/remove_from_25004.csv")
r$id <- paste(r$remove, r$RouteNumber, sep=".")
r <- unique(r$id)

d$id <- paste(d$ref, d$RouteNumber, sep=".")
d <- d %>% filter(!id %in% r)



load("~/space-time/final datasets/SR3_mean_open/richness_open_all.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))

pdf("richness_open_regression_all.pdf", height = 10)

par(mfrow=c(3,2),
    omi=c(0.3, 0.3, 0.3, 0.3))

for(i in 1:33){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#2c7bb6"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#ED432D"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  n <- unique(d_space$Obs_ID)
  n2 <- unique(d_time$Obs_ID)
  
  x.seq <- seq( from=min(d_filter$Cover_std), to = max(d_filter$Cover_std), length.out = 30 )
  
  
  mu.space <- sapply(x.seq,  function(x) mean( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) 
  mu.time <- sapply(x.seq,  function(x) mean( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) 
  
  ci.space <- sapply(x.seq,  function(x) PI( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) ) 
  ci.time <- sapply(x.seq,  function(x) PI( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) 
  
  
  plot(d_filter$Cover_std, d_filter$Richness_avg, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.25, cex.main = 1.5, 
       main = paste("Region", i), xlab = "Percent forest cover", ylab = "Mean species richness")
  
  axis(1, at = c(min(d_filter$Cover_std), median(d_filter$Cover_std), max(d_filter$Cover_std)), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.25)
  axis(2, cex.axis = 1.25)
  line <- d %>% filter(Region == 3)
  abline(v = median(line$Cover_std), lty = 2, col = alpha("black", 0.2))
  
  shade( ci.space, x.seq, col = alpha("#ED432D", 0.05))
  shade( ci.time, x.seq, col = alpha("#2c7bb6", 0.05))
  
  
  lines( x.seq, mu.time, col = "#2c7bb6", lwd = 1.5)
  lines( x.seq, mu.space, col = "#ED432D", lwd = 1.5 )
  
}
dev.off()



########## EVENNESS ###############

d <- read.csv("~/space-time/final datasets/evenness_FINAL.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

load("~/space-time/final datasets/DIV1_total/evenness_total_may3.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))


pdf("evenness_total_regression.pdf", height = 10)

par(mfrow=c(3,2),
    omi=c(0.3, 0.3, 0.3, 0.3))

for(i in 1:33){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#2c7bb6"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#ED432D"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  n <- unique(d_space$Obs_ID)
  n2 <- unique(d_time$Obs_ID)
  
  x.seq <- seq( from=min(d_filter$Cover_std), to = max(d_filter$Cover_std), length.out = 30 )
  
  
  mu.space <- sapply(x.seq,  function(x) mean( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) )
  mu.time <- sapply(x.seq,  function(x) mean( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) )
  
  ci.space <- sapply(x.seq,  function(x) PI( draws$a[, i, 2] + mean(draws$observer[, n]) + draws$b_space[, i] * x ) )
  ci.time <- sapply(x.seq,  function(x) PI( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) 
  
  
  plot(d_filter$Cover_std, d_filter$evenness, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.25, cex.main = 1.5, 
       main = paste("Region", i), xlab = "Percent forest cover", ylab = "Total abundance")
  
  axis(1, at = c(min(d_filter$Cover_std), median(d_filter$Cover_std), max(d_filter$Cover_std)), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.25)
  axis(2, cex.axis = 1.25)
  line <- d %>% filter(Region == 3)
  abline(v = median(line$Cover_std), lty = 2, col = alpha("black", 0.2))
  
  shade( ci.space, x.seq, col = alpha("#ED432D", 0.05))
  shade( ci.time, x.seq, col = alpha("#2c7bb6", 0.05))
  
  
  lines( x.seq, mu.time, col = "#2c7bb6", lwd = 1.5)
  lines( x.seq, mu.space, col = "#ED432D", lwd = 1.5 )
  
}
dev.off()
