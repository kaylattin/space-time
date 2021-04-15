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
d <- read.csv("whole_dataset_ND_version4.csv")
hist(d$Count)
d <- read.csv("whole_dataset_diversity_mar2021_version4.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))

draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))

par(mfrow=c(3,2))
par(mai=c(0.5, 1, 0.5, 1))

for(i in 1:27){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#192e40"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#D1495B"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  n <- nrow(d_space)
  
  x.seq <- seq( from=min(scale(d$Forest.cover)), to = max(scale(d$Forest.cover)), length.out = 30 )
  

  mu.space <- sapply(x.seq,  function(x) mean( draws$a[, i, 2] + mean(draws$observer[, d_space$Obs_ID[1:n]]) + draws$b_space[, i] * x ) )
  mu.time <- sapply(x.seq,  function(x)  mean( draws$a[, i, 1] + mean(draws$observer[, d_time$Obs_ID[1:15]]) + draws$b_time[, i] * x ) ) 

  ci.space <- sapply(x.seq,  function(x)  PI( draws$a[, i, 2] + mean(draws$observer[, d_space$Obs_ID[1:n]]) + draws$b_space[, i] * x ) ) 
  ci.time <- sapply(x.seq,  function(x)  PI( draws$a[, i, 1] + mean(draws$observer[, d_time$Obs_ID[1:15]]) + draws$b_time[, i] * x ) ) 
  
  
  plot(scale(d_filter$Forest.cover), d_filter$Hprime, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 2, 
       main = paste("Region", i), xlab = "% forest cover", ylab = "Shannon diversity")
  
  axis(1, at = c(min(scale(d_filter$Forest.cover)), median(scale(d_filter$Forest.cover)), max(scale(d_filter$Forest.cover))), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.5)
  axis(2, cex.axis = 1.5)
  
  shade( ci.time, x.seq, col = alpha("#192e40", 0.1))
  shade( ci.space, x.seq, col = alpha("#D1495B", 0.2))
  
  
  lines( x.seq, mu.time, col = "#192e40", lwd = 1.5)
  lines( x.seq, mu.space, col = "#D1495B", lwd = 1.5 )
  
}

d <- read.csv("whole_dataset_ta_mar2021_version4.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))

draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))

par(mfrow=c(3,2))
par(mai=c(0.5, 1, 0.5, 1))

for(i in 1:27){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#192e40"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#D1495B"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  n <- nrow(d_space)
  
  x.seq <- seq( from=min(scale(d$Forest.cover)), to = max(scale(d$Forest.cover)), length.out = 30 )
  
  
  mu.space <- sapply(x.seq,  function(x) exp( mean( draws$a[, i, 2] + mean(draws$observer[, d_space$Obs_ID[1:n]]) + draws$b_space[, i] * x ) ) ) 
  mu.time <- sapply(x.seq,  function(x) exp( mean( draws$a[, i, 1] + mean(draws$observer[, d_time$Obs_ID[1:15]]) + draws$b_time[, i] * x ) ) )
  
  ci.space <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 2] + mean(draws$observer[, d_space$Obs_ID[1:n]]) + draws$b_space[, i] * x ) ) )
  ci.time <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 1] + mean(draws$observer[, d_time$Obs_ID[1:15]]) + draws$b_time[, i] * x ) ) )
  
  
  plot(scale(d_filter$Forest.cover), d_filter$TotalAbundance, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 2, 
       main = paste("Region", i), xlab = "% forest cover", ylab = "Total Abundance")
  
  axis(1, at = c(min(scale(d_filter$Forest.cover)), median(scale(d_filter$Forest.cover)), max(scale(d_filter$Forest.cover))), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.5)
  axis(2, cex.axis = 1.5)
  
  shade( ci.time, x.seq, col = alpha("#192e40", 0.1))
  shade( ci.space, x.seq, col = alpha("#D1495B", 0.2))
  
  
  lines( x.seq, mu.time, col = "#192e40", lwd = 1.5)
  lines( x.seq, mu.space, col = "#D1495B", lwd = 1.5 )
  
}

d <- read.csv("total_richness_dataset.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))
d$Cover_std <- as.vector(scale(d$Forest.cover))

draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))

par(mfrow=c(3,2))
par(mai=c(0.5, 1, 0.5, 1))

for(i in 1:21){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#192e40"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#D1495B"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  n <- unique(d_space$Obs_ID)
  n2 <- unique(d_time$Obs_ID)

  x.seq <- seq( from=min(d_filter$Cover_std), to = max(d_filter$Cover_std), length.out = 30 )
  
  
  mu.space <- sapply(x.seq,  function(x) exp(mean( draws$a[, i, 2] + draws$observer[, n] + draws$b_space[, i] * x ) ) )
  mu.time <- sapply(x.seq,  function(x) exp(mean( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) )
  
  ci.space <- sapply(x.seq,  function(x) exp(PI( draws$a[, i, 2] + draws$observer[, n] + draws$b_space[, i] * x ) ) )
  ci.time <- sapply(x.seq,  function(x) exp(PI( draws$a[, i, 1] + draws$observer[, n2] + draws$b_time[, i] * x ) ) )
  
  
  plot(d_filter$Cover_std, d_filter$Richness, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 2, 
       main = paste("Region", i), xlab = "% forest cover", ylab = "Species Richness")
  
  axis(1, at = c(min(d_filter$Cover_std), median(d_filter$Cover_std), max(d_filter$Cover_std)), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.5)
  axis(2, cex.axis = 1.5)
  line <- d %>% filter(Region == 1)
  abline(v = median(line$Cover_std), lty = 2, col = alpha("black", 0.1))
  
  shade( ci.space, x.seq, col = alpha("#D1495B", 0.1))
  shade( ci.time, x.seq, col = alpha("#192e40", 0.1))

  
  lines( x.seq, mu.time, col = "#192e40", lwd = 1.5)
  lines( x.seq, mu.space, col = "#D1495B", lwd = 1.5 )
  
}







d <- read.csv("whole_dataset_diversity_mar2021_version4.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))

draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "observer"))

par(mfrow=c(3,2))
par(mai=c(0.5, 1, 0.5, 1))

for(i in 1:27){
  d_filter <- d %>% filter(Region == i)
  d_filter$Colour= "#192e40"
  d_filter$Shape = 16
  # Set new column values to appropriate colours
  d_filter$Colour[d_filter$space.time == 2] = "#D1495B"
  d_filter$Shape[d_filter$space.time == 2] = 17
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  n <- nrow(d_space)
  
  x.seq <- seq( from=min(scale(d$Forest.cover)), to = max(scale(d$Forest.cover)), length.out = 30 )
  
  
  mu.space <- sapply(x.seq,  function(x) mean( draws$a[, i, 2] +  draws$b_space[, i] * x ) )
  mu.time <- sapply(x.seq,  function(x)  mean( draws$a[, i, 1] + draws$b_time[, i] * x ) ) 
  
  ci.space <- sapply(x.seq,  function(x)  PI( draws$a[, i, 2] + draws$b_space[, i] * x ) ) 
  ci.time <- sapply(x.seq,  function(x)  PI( draws$a[, i, 1] + draws$b_time[, i] * x ) ) 
  
  
  plot(scale(d_filter$Forest.cover), d_filter$Hprime, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 2, 
       main = paste("Region", i), xlab = "% forest cover", ylab = "Shannon diversity")
  
  axis(1, at = c(min(scale(d_filter$Forest.cover)), median(scale(d_filter$Forest.cover)), max(scale(d_filter$Forest.cover))), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.5)
  axis(2, cex.axis = 1.5)
  
  shade( ci.time, x.seq, col = alpha("#192e40", 0.1))
  shade( ci.space, x.seq, col = alpha("#D1495B", 0.2))
  
  
  lines( x.seq, mu.time, col = "#192e40", lwd = 1.5)
  lines( x.seq, mu.space, col = "#D1495B", lwd = 1.5 )
  
}
