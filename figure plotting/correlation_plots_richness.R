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

load("~/space-time/final datasets/SR1_total/total_richness_apri15_FINAL.RData")
b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]


ta <- data.frame(x, y)
ta$region <- seq(1:21)
ta <- merge(ta, g, by = "region")


## all regions
rich_total <- ggplot(ta, mapping = aes(x,y), color = Location) + 
  geom_point(
    aes(colour = as.factor(Location)),
    alpha = 0.8,
    size = 5
  ) +
  scale_color_manual(values = c("Southeastern US" = "#41b6c4",
                                "Midwestern US"="#78c679",
                                "Eastern US Coast"="#253494")
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


rich_total <- rich_total +  theme(legend.position = "none")  + labs(title = "Total species richness")

## mean - forest

load("~/space-time/final datasets/SR2_mean_forest/mean_richness_forest_FINALV2.RData")
b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]


ta <- data.frame(x, y)
ta$region <- seq(1:21)
ta <- merge(ta, g, by = "region")

## all regions
richf <- ggplot(ta, mapping = aes(x,y), color = Location) + 
  geom_point(
    aes(colour = as.factor(Location)),
    alpha = 0.8,
    size = 5
  ) +
  scale_color_manual(values = c("Southeastern US" = "#41b6c4",
                                "Midwestern US"="#78c679",
                                "Eastern US Coast"="#253494")
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



richf <- richf + theme(legend.position = "none")  + labs(title = "Mean forest bird species richness")


## mean - open
load("~/space-time/final datasets/SR3_mean_open/mean_richness_open_only_FINAL.RData")

b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")


x <- b_space$summary[,1]
y <- b_time$summary[,1]


ta <- data.frame(x, y)
ta$region <- seq(1:21)
ta <- merge(ta, g, by = "region")

## all regions
richo <- ggplot(ta, mapping = aes(x,y), color = Location) + 
  geom_point(
    aes(colour = as.factor(Location)),
    alpha = 0.8,
    size = 5
  ) +
  scale_color_manual(values = c("Southeastern US" = "#41b6c4",
                                "Midwestern US"="#78c679",
                                "Eastern US Coast"="#253494")
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


richo <- richo + theme(legend.box = "vertical", legend.position = "right") + labs(title = "Mean non-forest bird species richness", color = "Geographical region")
richo


all <- ggarrange(rich_total + theme(legend.position="none"),
                 richf + theme(legend.position="none"), 
                 richo + theme(legend.position="right"),
                 ncol = 1, nrow = 3)

ggsave(filename = "richness_correlation_plots.png", device = "png", plot = all,
       width = 20, height = 30, units = "cm")

