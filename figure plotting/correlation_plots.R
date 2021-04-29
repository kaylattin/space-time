library(tidyverse)
library(ggpubr)
library(rstan)
library(ggrepel)
library(gridExtra)
library(rethinking)


setwd("/Users/kayla/Documents/space-time/final datasets")


# g <- read.csv("geographical_location.csv")
# g$region <- as.integer(as.factor(g$RouteNumber))


### abundance correlation plots

## total

load("~/space-time/final datasets/TA1_total/ta_total_apr29.RData")


b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]



## set everything up
niterations = 8000
N = 200

bs <- rstan::extract(stanfit, "b_space")
bt <- rstan::extract(stanfit, "b_time")

b_space <- bs$b_space
b_time <- bt$b_time

# prediction fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# initialize
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)

# for every iteration, calculate correlation of b_space (across 33 regions) and b_time (across 33 regions)
# then predict values using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,])
  
  intercept[i] = mlm$coefficients[[1]] # use mlm or lmodel2?
  slope[i] = mlm$coefficients[[2]]
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) 
  
}

mu.mean <- apply( mu, 2, mean)
ci.mean <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean, pred_data)

## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))


conf <- data.frame(pred_data, mu.mean)



## all regions
ta_total <- ggplot(ta, mapping = aes(x,y)) + 
  geom_point(
    colour = "#8c96c6",
    alpha = 0.8,
    size = 5
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    alpha = 0.5,
    size = 0.5
  ) +
  geom_abline(
    intercept = int_avg,
    slope = slope_avg,
    colour = "#8c96c6",
    size = 1
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()


ta_total <- ta_total +  theme()  + labs(title = "Total bird abundance") +
  geom_ribbon(data = conf, mapping = aes(x = pred_data, y = mu.mean,
                                         xmin = min(pred_data), xmax = max(pred_data),
                                         ymin = ci.mean[1,],
                                         ymax = ci.mean[2,]),
              fill = "#8c96c6",
              alpha = 0.1)
ta_total

## mean - forest

load("~/space-time/final datasets/TA2_mean_forest/ta_forest_apr28.RData")

b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]



## set everything up
niterations = 8000
N = 200

bs <- rstan::extract(stanfit, "b_space")
bt <- rstan::extract(stanfit, "b_time")

b_space <- bs$b_space
b_time <- bt$b_time

# prediction fake data
pred_data = seq(from = min(x), to = max(x), length.out = 200 )

# initialize
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)

# for every iteration, calculate correlation of b_space (across 33 regions) and b_time (across 33 regions)
# then predict values using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,])
  
  intercept[i] = mlm$coefficients[[1]] # use mlm or lmodel2?
  slope[i] = mlm$coefficients[[2]]
  
  mu[i,] <- sapply(pred_data,  function(x) unlist(intercept[i]) + unlist(slope[i]) * x ) 
  
}

mu.mean <- apply( mu, 2, mean)
ci.mean2 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean, pred_data)

## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))


conf2 <- data.frame(pred_data, mu.mean)


## all regions
taf <- ggplot(ta, mapping = aes(x,y)) + 
  geom_point(
    colour = "#f768a1",
    alpha = 0.8,
    size = 5
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    alpha = 0.5,
    size = 0.5
  ) +
  geom_abline(
    intercept = int_avg,
    slope = slope_avg,
    colour = "#f768a1",
    size = 1
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()


taf <- taf + theme() + labs(title = "Mean forest bird abundance") +
  geom_ribbon(data = conf2, mapping = aes(x = pred_data, y = mu.mean,
                                         xmin = min(pred_data), xmax = max(pred_data),
                                         ymin = ci.mean2[1,],
                                         ymax = ci.mean2[2,]),
              fill = "#f768a1",
              alpha = 0.1)
taf

## mean - open
load("~/space-time/final datasets/TA3_mean_open/ta_open_apr28.RData")

b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]



## set everything up
niterations = 8000
N = 200

bs <- rstan::extract(stanfit, "b_space")
bt <- rstan::extract(stanfit, "b_time")

b_space <- bs$b_space
b_time <- bt$b_time

# prediction fake data
pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )

# initialize
intercept <- vector("list")
slope <- vector("list")
pred_lines <- vector("list")
mu <- matrix(nrow=8000, ncol=200)

# for every iteration, calculate correlation of b_space (across 33 regions) and b_time (across 33 regions)
# then predict values using the intercept & slope for each iteration
for (i in 1:niterations){
  
  mlm = lm(b_time[i,] ~ b_space[i,])
  
  intercept[i] = mlm$coefficients[[1]] # use mlm or lmodel2?
  slope[i] = mlm$coefficients[[2]]
  
  mu[i,] <- sapply(pred_data,  function(x) unlist(intercept[i]) + unlist(slope[i]) * x ) 
  
}

mu.mean <- apply( mu, 2, mean)
ci.mean3 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean, pred_data)

## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))


conf3 <- data.frame(pred_data, mu.mean)


## all regions
tao <- ggplot(ta, mapping = aes(x,y)) + 
  geom_point(
    colour = "#7a0177",
    alpha = 0.8,
    size = 5
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    alpha = 0.5,
    size = 0.5
  ) +
  geom_abline(
    intercept = int_avg,
    slope = slope_avg,
    colour = "#7a0177",
    size = 1
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()


tao <- tao + theme() + labs(title = "Mean non-forest bird abundance") +
  geom_ribbon(data = conf3, mapping = aes(x = pred_data, y = mu.mean,
                                         xmin = min(pred_data), xmax = max(pred_data),
                                         ymin = ci.mean3[1,],
                                         ymax = ci.mean3[2,]),
              fill = "#7a0177",
              alpha = 0.1)
tao

all <- ggarrange(ta_total + theme(legend.position="none"),
                 taf + theme(legend.position="none"), 
                 tao + theme(legend.position="right"),
                 ncol = 1, nrow = 3)


all 

setwd("/Users/kayla/Documents/space-time/figure plotting")
ggsave(filename = "abundance_correlation_plots_apr29.png", device = "png", plot = all,
       width = 20, height = 30, units = "cm")

