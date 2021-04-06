library(tidyverse)
library(ggpubr)
library(rstan)
library(bayesplot)

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

par(mfrow=c(5,2))

for(i in 1:27){
  d_filter <- d %>% filter(Region == i)
  
  d_space <- d_filter %>% filter(space.time == 2)
  d_time <- d_filter %>% filter(space.time == 1)

  int <- at_rg$summary[i,1]
  ins <- as_rg$summary[i,1]
  b_space <- bs_rg$summary[i,1]
  b_time <- bt_rg$summary[i,1]
  
  
  plot(scale(d_space$Forest.cover), d_space$Count,
       xlab = "% forest cover", ylab = "Species abundance")
  
  x <- scale(d$Forest.cover)
  
  curve( exp(ins + b_space * x), add = TRUE)
  
  mtext(paste(i,  "- space; slope:", b_space))
  
  plot(scale(d_time$Forest.cover), d_time$Count,
       xlab = "% forest cover", ylab = "Species abundance")
  
  x <- scale(d$Forest.cover)
  
  curve( exp(int + b_time* x), add = TRUE)
  
  mtext(paste(i, "- time; slope:", b_time))
  
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

