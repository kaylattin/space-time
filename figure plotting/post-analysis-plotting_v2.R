library(tidyverse)
library(ggpubr)
library(rstan)
library(bayesplot)
library(rethinking)
library(scales)

load("main_model_mar31.RData")

draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time",
                                          "b_dif_sp_mean",
                                          "b_dif_reg_mean",
                                          "b_time_mean_sp",
                                          "b_time_mean_rg",
                                          "b_space_mean_sp",
                                          "b_space_mean_rg",
                                          "a_space_mean_rg",
                                          "a_time_mean_rg"))


summ <- rstan::summary(stanfit, pars = c("a", "b_space", "b_time",
                                         "b_dif_sp_mean",
                                         "b_dif_reg_mean",
                                         "b_time_mean_sp",
                                         "b_time_mean_rg",
                                         "b_space_mean_sp",
                                         "b_space_mean_rg",
                                         "a_space_mean_rg",
                                         "a_time_mean_rg"))

bs <- rstan::summary(stanfit, pars = "b_space")
bt <- rstan::summary(stanfit, pars = "b_time")

bs_sp <- rstan::summary(stanfit, pars = "b_space_mean_sp")
bs_rg <- rstan::summary(stanfit, pars = "b_space_mean_rg")

bt_sp <- rstan::summary(stanfit, pars = "b_time_mean_sp")
bt_rg <- rstan::summary(stanfit, pars = "b_time_mean_rg")

as_rg <- rstan::summary(stanfit, pars = "a_space_mean_rg")
at_rg <- rstan::summary(stanfit, pars = "a_time_mean_rg")


x <- bs$summary[,1]
x[which(x == 0)] <- NA
x <- na.omit(x)

y <- bt$summary[,1]
y[which(y == 0)] <- NA
y <- na.omit(y)

spreg_df <- data.frame(x, y)

## Full species-regions
spreg <- ggplot(spreg_df, mapping = aes(x, y)) + 
  geom_point(
    colour = "#00798C",
    alpha = 0.7,
    size = 3
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    size = 0.5
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()

spreg <- spreg + theme(legend.position = "none")
spreg


## Species slope means, summarized across regions

x <- bs_sp$summary[,1]
y <- bt_sp$summary[,1]



d$Species <- as.integer(as.factor(d$BBL))
sp_list <- d %>% distinct(Species, BBL)

sp_df <- data.frame(x, y, sp_list$BBL)

species <- ggplot(sp_df, mapping = aes(x, y)) + 
  geom_point(
    colour = "#00798C",
    alpha = 0.7,
    size = 3
  ) +
  geom_text(
    aes(label = sp_list.BBL),
    colour = "#00798C",
    size = 3,
    hjust = -0.1,
    vjust = -0.4
    
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    size = 0.5
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()

species <- species + theme(legend.position = "none")
species



## Region slope means, summarized across species


x <- bs_rg$summary[,1]
y <- bt_rg$summary[,1]
rg <- seq(1:27)

reg_df <- data.frame(x, y, rg)


reg <- ggplot(reg_df, mapping = aes(x, y)) + 
  geom_point(
    colour = "#00798C",
    alpha = 0.7,
    size = 3
  ) +
  geom_text(
    aes(label = rg),
    colour = "#00798C",
    size = 3,
    hjust = -0.2,
    vjust = -0.3
    
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    size = 0.5
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()

reg <- reg + theme(legend.position = "none")
reg


all <- ggarrange(spreg, species, reg, 
                 labels = c("across species-regions", "across species", "across regions"),
                 ncol = 1, nrow = 3)
all

### Plot by region, cycling through 

d <- read.csv("~/space-time/final datasets/whole_dataset_ND_version4.csv")
d$Region <- as.integer(as.factor(d$ref))
d$Obs_ID <- as.integer(as.factor(d$ObsN))

draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "obs_offset",
                                          "b_dif_sp_mean",
                                          "b_dif_reg_mean",
                                          "b_time_mean_sp",
                                          "b_time_mean_rg",
                                          "b_space_mean_sp",
                                          "b_space_mean_rg",
                                          "a_space_mean_rg",
                                          "a_time_mean_rg"))

par(mfrow=c(3,2))
par(mai=c(0.5, 1, 0.4, 1))

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
  
  x.seq <- seq( from=min(scale(d_filter$Forest.cover)), to = max(scale(d_filter$Forest.cover)), length.out = 30 )
  
  
  mu.space <- sapply(x.seq,  function(x) exp( mean( draws$a_space_mean_rg[, i] + mean(draws$obs_offset[, d_space$Obs_ID[1:n]]) + draws$b_space_mean_rg[, i] * x ) ) ) 
  mu.time <- sapply(x.seq,  function(x) exp( mean( draws$a_time_mean_rg[, i] + mean(draws$obs_offset[, d_time$Obs_ID[1:15]]) + draws$b_time_mean_rg[, i] * x ) ) )
  
  ci.space <- sapply(x.seq,  function(x) exp( PI( draws$a_space_mean_rg[, i] + mean(draws$obs_offset[, d_space$Obs_ID[1:n]]) + draws$b_space_mean_rg[, i] * x ) ) )
  ci.time <- sapply(x.seq,  function(x) exp( PI( draws$a_time_mean_rg[, i] + mean(draws$obs_offset[, d_time$Obs_ID[1:15]]) + draws$b_time_mean_rg[, i] * x ) ) )
  
  
  plot(scale(d_filter$Forest.cover), d_filter$Count, col = alpha(d_filter$Colour, 0.6), pch = d_filter$Shape,
       xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 2, 
       main = paste("Region", i), xlab = "% forest cover", ylab = "Species Abundance")
  
  axis(1, at = c(min(scale(d_filter$Forest.cover)), median(scale(d_filter$Forest.cover)), max(scale(d_filter$Forest.cover))), 
       labels = c(round(min(d_filter$Forest.cover),2), round(median(d_filter$Forest.cover),2), round(max(d_filter$Forest.cover),2)),
       cex.axis = 1.5)
  axis(2, cex.axis = 1.5)
  
  shade( ci.time, x.seq, col = alpha("#192e40", 0.1))
  shade( ci.space, x.seq, col = alpha("#D1495B", 0.2))
  
  
  lines( x.seq, mu.time, col = "#192e40", lwd = 1.5)
  lines( x.seq, mu.space, col = "#D1495B", lwd = 1.5 )
}


### B-diff plots

b_dif_sp <- as.matrix(stanfit, pars = "b_dif_sp_mean")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_reg_mean")

### main model - by species
color_scheme_set("teal")
mcmc_intervals(b_dif_sp)


### main model - by region
color_scheme_set("teal")
mcmc_intervals(b_dif_rg)

