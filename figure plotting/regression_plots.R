## load up stuff
setwd("/Users/kayla/Documents/space-time/final datasets")
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)
library(scales)

rm(list = ls())
gc()


d <- read.csv("whole_dataset_diversity_mar2021_version4.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))

par(mfrow=c(5,2))

for(i in 1:27){
  d_filter <- d %>% filter(Region == i)
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  
  
  b_space <- summary(stanfit, pars = "b_space")
  b_time <- summary(stanfit, pars = "b_time")
  a <- summary(stanfit, pars = "a")
  
  
  offset <- summary(stanfit, pars = "obs_offset")
  
  n <- nrow(d_space)
  
  obs <- offset$summary[d_space$Obs_ID[1:n], 1]
  obs <- unique(obs)
  obs_mean <- mean(obs)
  
  
  obst <- offset$summary[d_time$Obs_ID[1:15], 1]
  obst <- unique(obst)
  obst_mean <- mean(obst)
  
  
  a_space <- a$summary[c(rep(FALSE,1),TRUE), 1]
  a_time <- a$summary[c(TRUE, rep(FALSE,1)), 1]
  
  
  int <- a_time[i]
  ins <- a_space[i]
  bs <- (b_space$summary[i,1] / sd(d$Forest.cover)) # unstandardize the standardized slopes
  bt <- (b_time$summary[i,1] / sd(d$Forest.cover))
  
 
  plot(d_space$Forest.cover, d_space$Hprime,
       xlab = "% forest cover", ylab = "Diversity")
  
  x <- d$Forest.cover
  
  curve( ((ins + obs_mean) + (bs * x)), add = TRUE)
  
  mtext(paste(i,  "- space; slope:", bs))
  
  plot(d_time$Forest.cover, d_time$Hprime,
       xlab = "% forest cover", ylab = "Diversity")
  
  x <- d$Forest.cover
  
  curve( ((int + obst_mean) + (bt * x)), add = TRUE)
  
  mtext(paste(i, "- time; slope:", bt))
  
}



d <- read.csv("whole_dataset_ta_mar2021_version4.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))

par(mfrow=c(5,2))

for(i in 1:27){
  d_filter <- d %>% filter(Region == i)
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  
  b_space <- summary(stanfit, pars = "b_space")
  b_time <- summary(stanfit, pars = "b_time")
  a <- summary(stanfit, pars = "a")
  
  
  offset <- summary(stanfit, pars = "obs_offset")
  
  n <- nrow(d_space)
  
  obs <- offset$summary[d_space$Obs_ID[1:n], 1]
  obs <- unique(obs)
  obs_mean <- mean(obs)
  
  
  obst <- offset$summary[d_time$Obs_ID[1:15], 1]
  obst <- unique(obst)
  obst_mean <- mean(obst)
  
  
  a_space <- a$summary[c(rep(FALSE,1),TRUE), 1]
  a_time <- a$summary[c(TRUE, rep(FALSE,1)), 1]
  
  
  int <- a_time[i]
  ins <- a_space[i]
  bs <- (b_space$summary[i,1] / sd(d$Forest.cover)) # unstandardize the standardized slopes
  bt <- (b_time$summary[i,1] / sd(d$Forest.cover))
  
  
  plot(d_space$Forest.cover, d_space$TotalAbundance,
       xlab = "% forest cover", ylab = "Total Abundance")
  
  x <- d$Forest.cover
  
  curve( exp((ins + obs_mean) + (bs * x)), add = TRUE)
  
  mtext(paste(i,  "- space; slope:", bs))
  
  plot(d_time$Forest.cover, d_time$TotalAbundance,
       xlab = "% forest cover", ylab = "Total Abundance")
  
  x <- d$Forest.cover
  
  curve( exp((int + obst_mean) + (bt* x)), add = TRUE)
  
  mtext(paste(i, "- time; slope:", bt))
  
}



d <- read.csv("whole_dataset_richness_mar2021_version4.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))

par(mfrow=c(5,2))

for(i in 1:27){
  d_filter <- d %>% filter(Region == i)
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)
  
  b_space <- summary(stanfit, pars = "b_space")
  b_time <- summary(stanfit, pars = "b_time")
  a <- summary(stanfit, pars = "a")
  
  
  offset <- summary(stanfit, pars = "obs_offset")
  
  n <- nrow(d_space)
  
  obs <- offset$summary[d_space$Obs_ID[1:n], 1]
  obs <- unique(obs)
  obs_mean <- mean(obs)
  
  
  obst <- offset$summary[d_time$Obs_ID[1:15], 1]
  obst <- unique(obst)
  obst_mean <- mean(obst)
  
  
  a_space <- a$summary[c(rep(FALSE,1),TRUE), 1]
  a_time <- a$summary[c(TRUE, rep(FALSE,1)), 1]
  
  
  int <- a_time[i]
  ins <- a_space[i]
  bs <- b_space$summary[i,1] # unstandardize the standardized slopes
  bt <- b_time$summary[i,1]
  
  
  plot(scale(d_space$Forest.cover), d_space$Richness,
       xlab = "% forest cover", ylab = "Species Richness")
  
  x <- scale(d$Forest.cover)
  
  curve( exp(ins + (bs * x) + obs_mean), add = TRUE)
  
  mtext(paste(i,  "- space; slope:", bs))
  
  plot(scale(d_time$Forest.cover), d_time$Richness,
       xlab = "% forest cover", ylab = "Species Richness")
  
  x <- scale(d$Forest.cover)
  
  curve( exp(int + (bt* x) + obst_mean), add = TRUE)
  
  mtext(paste(i, "- time; slope:", bt))
  
}



#### THIS IS THE GOOD ONE! ---------------------------------------------------------
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
  
  
  mu.space <- sapply(x.seq,  function(x) exp( mean( draws$a[, i, 2] + mean(draws$obs_offset[, d_space$Obs_ID[1:n]]) + draws$b_space[, i] * x ) ) ) 
  mu.time <- sapply(x.seq,  function(x) exp( mean( draws$a[, i, 1] + mean(draws$obs_offset[, d_time$Obs_ID[1:15]]) + draws$b_time[, i] * x ) ) )
  
  ci.space <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 2] + mean(draws$obs_offset[, d_space$Obs_ID[1:n]]) + draws$b_space[, i] * x ) ) )
  ci.time <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 1] + mean(draws$obs_offset[, d_time$Obs_ID[1:15]]) + draws$b_time[, i] * x ) ) )
  
  
  plot(scale(d_filter$Forest.cover), d_filter$Richness, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 2, 
       main = paste("Region", i), xlab = "% forest cover", ylab = "Species Richness")
  
  axis(1, at = c(min(scale(d_filter$Forest.cover)), median(scale(d_filter$Forest.cover)), max(scale(d_filter$Forest.cover))), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.5)
  axis(2, cex.axis = 1.5)
  
  shade( ci.time, x.seq, col = alpha("#192e40", 0.1))
  shade( ci.space, x.seq, col = alpha("#D1495B", 0.2))


  lines( x.seq, mu.time, col = "#192e40", lwd = 1.5)
  lines( x.seq, mu.space, col = "#D1495B", lwd = 1.5 )
  
}



### DUMP - BRAINSTORMING AND STUFF ~



#
#
#
#

plot(scale(d_filter$Forest.cover), d_filter$Richness, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
     xaxt = "n", xlab = "% forest cover", ylab = "Species Richness")
axis(1, at = c(min(scale(d_filter$Forest.cover)), median(scale(d_filter$Forest.cover)), max(scale(d_filter$Forest.cover))), 
     labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)))

draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "obs_offset"))

link_space <- function(draws, region, forest)
{
  a <- draws$a
  a_space <- a[,region,2]
  
  b <- draws$b_space
  b_space <- b[,region]
  
  pred_forest <- a_space + b_space * forest
  pred_forest <- rpois(length(a_space), pred_forest)
  return(pred_forest)
}
link_space(draws = draws, region = 1, forest = scale(d_space$Forest.cover))

mu.space <- vector("list")
mu.time <- vector("list")
x.seq <- seq( from=min(scale(d$Forest.cover)), to = max(scale(d$Forest.cover)), length.out = 30 )
for(i in 1:27){
mu.space[[i]] <- sapply(x.seq,  function(x) mean( exp( draws$a[, i, 2] + draws$b_space[, 1] * x ) ) )
mu.time[[i]] <- sapply(x.seq,  function(x) mean( exp( draws$a[, i, 1] + draws$b_time[, 1] * x ) ) )
}

ci.space <- vector("list")
ci.time <- vector("list")
for(i in 1:27){
  ci.space[[i]] <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 2] + draws$b_space[, 1] * x ) ) )
  ci.time[[i]] <- sapply(x.seq,  function(x) exp( PI( draws$a[, i, 1] + draws$b_time[, 1] * x ) ) )
}

d_filter <- d %>% filter(Region == 1)
d_filter$Colour= "#192e40"
d_filter$Shape = 16
# Set new column values to appropriate colours
d_filter$Colour[d_filter$space.time == 2] = "#D1495B"
d_filter$Shape[d_filter$space.time == 2] = 17

d_space <- d_filter %>% filter(space.time == 2)
d_time <- d_filter %>% filter(space.time == 1)

plot(scale(d_filter$Forest.cover), d_filter$Richness, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
     xlab = "% forest cover", ylab = "Species Richness")

lines( x.seq, mu.space[[1]], col = "#D1495B" )
shade( ci.space[[1]], x.seq, col = alpha("#D1495B", 0.2))

lines( x.seq, mu.time[[1]], col = "#192e40" )
shade( ci.time[[1]], x.seq, col = alpha("#192e40", 0.2))






data(cars)
m <- lm( dist ~ speed , cars )
p <- extract.samples( m )
x.seq <- seq( from=min(cars$speed)-1 , to=max(cars$speed)+1 , length.out=30 )
mu.ci <- sapply( x.seq , function(x) PI( p[,1] + p[,2]*x ) )
plot( dist ~ speed , cars )
abline( m )
shade( mu.ci , x.seq )