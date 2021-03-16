library(tidyverse)
library(ggpubr)
library(rstan)
setwd("/Users/kayla/Documents/space-time/final datasets")




######################
# SPECIES RICHNESS  #
####################



load("v4_richness_stanfit.RData")
draws_richness <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "B_TIME", "B_SPACE"))

save(draws_richness, file = "richness_draws.RData")

b <- summary(stanfit, pars = "b")
x <- b$summary[1:32,1]
y <- b$summary[33:64, 1]


b_rich <- data.frame(x, y)
b_rich$region <- seq(1:32)


## all regions
richness <- ggplot(b_rich, mapping = aes(y, x)) + 
  geom_point(
    colour = "#192e40",
    alpha = 0.7,
    size = 3
  ) +
  geom_text(
    aes(label = b_rich$region),
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

richness <- richness + theme(legend.position = "none")
richness








######################
#     DIVERSITY     #
####################


load("v4_diversity_stanfit_test.RData")
draws_div <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "B_TIME", "B_SPACE"))

save(draws_div, file = "diversity_draws.RData")

b <- summary(stanfit, pars = "b")
x <- b$summary[1:32,1]
y <- b$summary[33:64, 1]


b_div <- data.frame(x, y)
b_div$region <- seq(1:32)


## all regions
shannon <- ggplot(b_div, mapping = aes(y, x)) + 
  geom_point(
    colour = "#D1495B",
    alpha = 0.7,
    size = 3
  ) +
  geom_text(
    aes(label = b_div$region),
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

shannon <- shannon + theme(legend.position = "none")
shannon






######################
# TOTAL ABUNDANCE   #
####################

load("v4_ta_stanfit.RData")
draws_ta <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "B_TIME", "B_SPACE"))

save(draws_ta, file = "ta_draws.RData")

b <- summary(stanfit, pars = "b")
x <- b$summary[1:32,1]
y <- b$summary[33:64, 1]


b_ta <- data.frame(x, y)
b_ta$region <- seq(1:32)


## all regions
ta <- ggplot(b_ta, mapping = aes(y, x)) + 
  geom_point(
    colour = "#EDAE49",
    alpha = 0.7,
    size = 3
  ) +
  geom_text(
    aes(label = b_ta$region),
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

ta <- ta + theme(legend.position = "none")
ta



all <- ggarrange(richness, ta, shannon, 
                 labels = c("richness", "tot abund", "diversity"),
                 ncol = 1, nrow = 3)
all