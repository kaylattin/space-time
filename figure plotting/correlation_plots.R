library(tidyverse)
library(ggpubr)
library(rstan)
library(ggrepel)
library(gridExtra)


setwd("/Users/kayla/Documents/space-time/final datasets")


g <- read.csv("geographical_location.csv")
g$region <- as.integer(as.factor(g$RouteNumber))


### abundance correlation plots

## total

load("~/space-time/final datasets/total_ta.RData")
b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]


ta <- data.frame(x, y)
ta$region <- seq(1:21)
ta <- merge(ta, g, by = "region")

## all regions
ta_total <- ggplot(ta, mapping = aes(x,y), color = Location) + 
  geom_point(
    aes(colour = as.factor(Location)),
    alpha = 0.7,
    size = 5
  ) +
  scale_color_manual(values = c("Southeastern US" = "#f768a1",
                                "Midwestern US"="#8c96c6",
                                "Eastern US Coast"="#7a0177")
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


ta_total <- ta_total +  theme(legend.position = "none")  + labs(title = "Total bird abundance")
ta_total

## mean - forest

load("~/space-time/final datasets/TA2_mean_forest/mean_ta_forest.RData")
b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]


ta <- data.frame(x, y)
ta$region <- seq(1:21)
ta <- merge(ta, g, by = "region")

## all regions
taf <- ggplot(ta, mapping = aes(x,y), color = Location) + 
  geom_point(
    aes(colour = as.factor(Location)),
    alpha = 0.8,
    size = 5
  ) +
  scale_color_manual(values = c("Southeastern US" = "#f768a1",
                                "Midwestern US"="#8c96c6",
                                "Eastern US Coast"="#7a0177")
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



taf <- taf + theme(legend.position = "none")  + labs(title = "Mean forest bird abundance")


## mean - open
load("~/space-time/final datasets/TA3_mean_open/mean_ta_open.RData")

b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]


ta <- data.frame(x, y)
ta$region <- seq(1:21)
ta <- merge(ta, g, by = "region")

## all regions
tao <- ggplot(ta, mapping = aes(x,y), color = Location) + 
  geom_point(
    aes(colour = as.factor(Location)),
    alpha = 0.8,
    size = 5
  ) +
  scale_color_manual(values = c("Southeastern US" = "#f768a1",
                                "Midwestern US"="#8c96c6",
                                "Eastern US Coast"="#7a0177")
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


tao <- tao + theme(legend.box = "vertical", legend.position = "right") + labs(title = "Mean non-forest bird abundance", color = "Comparison region")
tao


all <- ggarrange(ta_total + theme(legend.position="none"),
                 taf + theme(legend.position="none"), 
                 tao + theme(legend.position="right"),
                 ncol = 1, nrow = 3)

ggsave(filename = "abundance_correlation_plots.png", device = "png", plot = all,
       width = 20, height = 30, units = "cm")

