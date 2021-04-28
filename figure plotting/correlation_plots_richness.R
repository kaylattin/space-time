library(tidyverse)
library(ggpubr)
library(rstan)
library(ggrepel)
library(gridExtra)


setwd("/Users/kayla/Documents/space-time/final datasets")

g <- read.csv("geographical_location.csv")
g$region <- as.integer(as.factor(g$RouteNumber))

### richness correlation plots

## total

load("~/space-time/final datasets/SR1_total/total_richness_apr28.RData")



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
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) 
  
}

mu.mean <- vector()

# now take the mean of each column for n = 200 to find mean predicted values across all iterations
for(i in 1:N){
mu.mean[i] <- mean(mu[,i])
}

plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)


## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)
ta <- merge(ta, g, by = "region")


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))


## all regions
rich_total <- ggplot(ta, mapping = aes(x,y)) + 
  geom_point(
    colour = "#78c679",
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
    colour = "#78c679",
    size = 1
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()


rich_total <- rich_total +  theme(legend.position = "none")  + labs(title = "Total species richness")
rich_total



## mean - forest

load("~/space-time/final datasets/SR2_mean_forest/richness_forest_apr28.RData")


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
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) 
  
}

mu.mean <- vector()

# now take the mean of each column for n = 200 to find mean predicted values across all iterations
for(i in 1:N){
  mu.mean[i] <- mean(mu[,i])
}

plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)


## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)
ta <- merge(ta, g, by = "region")


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))


## all regions
richf <- ggplot(ta, mapping = aes(x,y)) + 
  geom_point(
    colour = "#41b6c4",
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
    colour = "#41b6c4",
    size = 1
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()


richf <- richf + theme(legend.position = "none")  + labs(title = "Mean forest bird species richness")
richf

## mean - open
load("~/space-time/final datasets/SR3_mean_open/richness_open_apr28.RData")

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
  
  mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) 
  
}

mu.mean <- vector()

# now take the mean of each column for n = 200 to find mean predicted values across all iterations
for(i in 1:N){
  mu.mean[i] <- mean(mu[,i])
}

plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)


## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)
ta <- merge(ta, g, by = "region")


# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))



## all regions
richo <- ggplot(ta, mapping = aes(x,y)) +
  geom_point(
    colour = "#253494",
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
    colour = "#253494",
    size = 1
  ) +
  labs(
    x = "Space slope", 
    y = "Time slope",
    size = 4
  ) +
  theme_bw()


richo <- richo + theme(legend.box = "vertical", legend.position = "right") + labs(title = "Mean non-forest bird species richness", color = "Geographical region")
richo


all <- ggarrange(rich_total + theme(legend.position="none"),
                 richf + theme(legend.position="none"), 
                 richo + theme(legend.position="right"),
                 ncol = 1, nrow = 3)

ggsave(filename = "richness_correlation_plots_apr28.png", device = "png", plot = all,
       width = 20, height = 30, units = "cm")

