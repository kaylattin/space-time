library(tidyverse)
library(ggpubr)
library(rstan)
library(ggrepel)
library(gridExtra)
library(rethinking)


setwd("/Users/kayla/Documents/space-time/final datasets")

### richness correlation plots

## total

# load("~/space-time/final datasets/SR1_total/total_richness_apr29.RData")
# 
# 
# 
# b_space <- summary(stanfit, pars = "b_space")
# b_time <- summary(stanfit, pars = "b_time")
# a <- summary(stanfit, pars = "a")
# 
# 
# x <- exp(b_space$summary[,1])
# y <- exp(b_time$summary[,1])
# 
# 
# 
# ## set everything up
# niterations = 8000
# N = 200
# 
# bs <- rstan::extract(stanfit, "b_space")
# bt <- rstan::extract(stanfit, "b_time")
# 
# b_space <- bs$b_space
# b_time <- bt$b_time
# 
# # prediction fake data
# pred_data = seq(from = (min(x) - 0.05), to = (max(x) + 0.05), length.out = 200 )
# 
# # initialize
# intercept <- vector("list")
# slope <- vector("list")
# pred_lines <- vector("list")
# mu <- matrix(nrow=8000, ncol=200)
# 
# # for every iteration, calculate correlation of b_space (across 33 regions) and b_time (across 33 regions)
# # then predict values using the intercept & slope for each iteration
# for (i in 1:niterations){
#   
#   mlm = lm(exp(b_time[i,]) ~ exp(b_space[i,]))
#   
#   intercept[i] = mlm$coefficients[[1]] # use mlm or lmodel2?
#   slope[i] = mlm$coefficients[[2]]
#   
#   mu[i,] <- sapply(pred_data,  function(x) mean( unlist(intercept[i]) + unlist(slope[i]) * x ) ) 
#   
# }
# 
# mu.mean <- apply( mu, 2, mean)
# ci.mean <- apply( mu, 2, PI, prob = 0.95 )
# 
# 
# plot(x, y)
# lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
# shade( ci.mean, pred_data)
# 
# ## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
# ta <- data.frame(x, y)
# ta$region <- seq(1:33)
# 
# 
# # plot these into ggplot?
# int_avg <- mean(unlist(intercept))
# slope_avg <- mean(unlist(slope))
# 
# 
# conf <- data.frame(pred_data, mu.mean)
# 
# 
# ## all regions
# rich_total <- ggplot(ta, mapping = aes(x,y)) + 
#   geom_point(
#     colour = "#78c679",
#     alpha = 0.8,
#     aes(size = nsites)
#   ) +
#   geom_abline(
#     intercept = 0,
#     slope = 1,
#     linetype = "dashed",
#     alpha = 0.5,
#     size = 1
#   ) +
#   geom_abline(
#     intercept = int_avg,
#     slope = slope_avg,
#     colour = "#78c679",
#     size = 1
#   ) +
#   labs(
#     x = "Space slope", 
#     y = "Temporal slope",
#     size = 4
#   ) +
#   theme_classic()
# 
# rich_total <- rich_total +  theme(legend.position = "none")  + labs(title = "Total species richness") + geom_ribbon(data = conf, mapping = aes(x = pred_data, y = mu.mean,
#                                                                   xmin = min(pred_data), xmax = max(pred_data),
#                                                                   ymin = ci.mean[1,],
#                                                                   ymax = ci.mean[2,]),
#                                        fill = "#78c679",
#                                        alpha = 0.1)
# rich_total

## mean - forest
load("~/space-time/final datasets/SR2_mean_forest/richness_forest_all.RData")


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
ci.mean2 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean, pred_data)

## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)


r <- read.csv("~/space-time/final datasets/SR2_mean_forest/richness_forest_FINALall.csv")


s <- read.csv("~/space-time/data prep/remove_from_25004.csv")
s$id <- paste(s$remove, s$RouteNumber, sep=".")
s <- unique(s$id)

r$id <- paste(r$ref, r$RouteNumber, sep=".")
r <- r %>% filter(!id %in% s)

spatial <- r %>% filter(space.time == 2)
spatial_sites <- spatial %>% group_by(ref) %>% summarize(nsites = n_distinct(RouteNumber)) %>% select(ref, nsites)
spatial_sites$region <- as.integer(as.factor(spatial_sites$ref))

spatial <- r %>% filter(space.time == 2)
spatial_sites <- spatial %>% group_by(ref) %>% summarize(nsites = n_distinct(RouteNumber)) %>% select(ref, nsites)
spatial_sites$region <- as.integer(as.factor(spatial_sites$ref))

ta <- merge(ta, spatial_sites, by = "region")

# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))


conf2 <- data.frame(pred_data, mu.mean)


## all regions
p1 <- ggplot(ta, mapping = aes(x,y)) + 
  geom_point(
    colour = "#253494",
    alpha = 0.8,
    aes(size = nsites)
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
    x = "", 
    y = "",
    size = 4
  ) +
  theme_classic()


p1 <- p1 + theme(plot.margin = unit(c(1,1,1,1),"cm"),
                 plot.title = element_text(size = 18),
                 axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
                 axis.text = element_text(size = 12)) + labs(title = "") +
  labs(title = expression(paste("II. ", italic(N[spatial])," > ",italic(N[temporal]))), size = "Spatial sample size") +
  geom_ribbon(data = conf2, mapping = aes(x = pred_data, y = mu.mean,
                                           xmin = min(pred_data), xmax = max(pred_data),
                                           ymin = ci.mean2[1,],
                                           ymax = ci.mean2[2,]),
                fill = "#253494",
                alpha = 0.1)
p1

## mean - open
load("~/space-time/final datasets/SR3_mean_open/richness_open_all.RData")


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
ci.mean3 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean3, pred_data)

## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)

r <- read.csv("~/space-time/final datasets/SR3_mean_open/richness_open_FINALall.csv")


s <- read.csv("~/space-time/data prep/remove_from_25004.csv")
s$id <- paste(s$remove, s$RouteNumber, sep=".")
s <- unique(s$id)

r$id <- paste(r$ref, r$RouteNumber, sep=".")
r <- r %>% filter(!id %in% s)

spatial <- r %>% filter(space.time == 2)
spatial_sites <- spatial %>% group_by(ref) %>% summarize(nsites = n_distinct(RouteNumber)) %>% select(ref, nsites)
spatial_sites$region <- as.integer(as.factor(spatial_sites$ref))

spatial <- r %>% filter(space.time == 2)
spatial_sites <- spatial %>% group_by(ref) %>% summarize(nsites = n_distinct(RouteNumber)) %>% select(ref, nsites)
spatial_sites$region <- as.integer(as.factor(spatial_sites$ref))

ta <- merge(ta, spatial_sites, by = "region")

# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))


conf3 <- data.frame(pred_data, mu.mean)


## all regions
p2 <- ggplot(ta, mapping = aes(x,y)) +
  geom_point(
    colour = "#41b6c4",
    alpha = 0.8,
    aes(size = nsites)
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
    x = "Spatial slope", 
    y = "",
    size = 4
  ) +
  theme_classic()


p2 <- p2 + theme(plot.margin = unit(c(0,1,1,1),"cm"),
                 axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
                 axis.text = element_text(size = 12)) + labs(title = "") + labs(title = "", size = "Spatial sample size") +
  geom_ribbon(data = conf3, mapping = aes(x = pred_data, y = mu.mean,
                                         xmin = min(pred_data), xmax = max(pred_data),
                                         ymin = ci.mean3[1,],
                                         ymax = ci.mean3[2,]),
              fill = "#41b6c4",
              alpha = 0.1)
p2




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
ci.mean4 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean4, pred_data)

## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)

ta <- merge(ta, spatial_sites, by = "region")

# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))


conf4 <- data.frame(pred_data, mu.mean)


## all regions
p3 <- ggplot(ta, mapping = aes(x,y)) + 
  geom_point(
    colour = "#253494",
    alpha = 0.8,
    size = 3,
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
    x = "", 
    y = "Temporal slope",
    size = 4
  ) +
  theme_classic()


p3 <- p3 + theme(plot.margin = unit(c(1,1,1,1),"cm"),
                 plot.title = element_text(size = 18),
                 axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
                 axis.text = element_text(size = 12)) + labs(title = "") +
  labs(title = expression(paste("I. ", italic(N[spatial]),phantom() %~~% phantom(),italic(N[temporal]))), size = "Spatial sample size") +
  geom_ribbon(data = conf4, mapping = aes(x = pred_data, y = mu.mean,
                                          xmin = min(pred_data), xmax = max(pred_data),
                                          ymin = ci.mean4[1,],
                                          ymax = ci.mean4[2,]),
              fill = "#253494",
              alpha = 0.1)
p3

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
ci.mean5 <- apply( mu, 2, PI, prob = 0.95 )


plot(x, y)
lines( pred_data, mu.mean, col = "#2c7bb6", lwd = 1.5)
shade( ci.mean5, pred_data)

## Now move onto ggplot and just feed the intercept & slope in; compare to above (which is the correct way of doing it)
ta <- data.frame(x, y)
ta$region <- seq(1:33)

ta <- merge(ta, spatial_sites, by = "region")

# plot these into ggplot?
int_avg <- mean(unlist(intercept))
slope_avg <- mean(unlist(slope))


conf5 <- data.frame(pred_data, mu.mean)


## all regions
p4 <- ggplot(ta, mapping = aes(x,y)) +
  geom_point(
    colour = "#41b6c4",
    alpha = 0.8,
    size = 3,
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
    x = "Spatial slope", 
    y = "Temporal slope",
    size = 4
  ) +
  theme_classic()

p4 <- p4 + theme(plot.margin = unit(c(0,1,1,1),"cm"),
                 axis.title = element_text(size = 14), axis.title.x = element_text(vjust= -2 ), axis.title.y = element_text(vjust = 5), 
                 axis.text = element_text(size = 12)) + labs(title = "") +
  geom_ribbon(data = conf5, mapping = aes(x = pred_data, y = mu.mean,
                                          xmin = min(pred_data), xmax = max(pred_data),
                                          ymin = ci.mean5[1,],
                                          ymax = ci.mean5[2,]),
              fill = "#41b6c4",
              alpha = 0.1)
p4


all <- ggarrange(p3,
                 p1 + theme(legend.position="none"), 
                 p4,
                 p2 + theme(legend.position="none"),
                 ncol = 2, nrow = 2)

all

ggsave(filename = "richness_correlation_4plots_minimal.png", device = "png", plot = all,
       width = 30, height = 30, units = "cm")

