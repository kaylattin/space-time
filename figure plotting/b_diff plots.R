## load up stuff
setwd("/Users/kayla/Documents/space-time/final datasets")
library(tidyverse)
library(cmdstanr)
library(shinystan)
library(rstan)
library(ggpubr)
library(bayesplot)

rm(list = ls())
gc()


# plotting b_dif
load("v3_species_abundance_nd.RData")

b_dif_sp <- as.matrix(stanfit, pars = "b_dif_sp_mean")
b_dif_rg <- as.matrix(stanfit, pars = "b_dif_reg_mean")

### main model - by species
color_scheme_set("teal")
mcmc_intervals(b_dif_sp)


### main model - by region
color_scheme_set("teal")
mcmc_intervals(b_dif_rg)




### richness
load("v4_richness_stanfit.RData")
draws_richness <- rstan::extract(stanfit, pars = c("b_dif_rg"))


b_dif <- as.matrix(stanfit, pars = "b_dif_rg")
color_scheme_set("gray")
mcmc_intervals(b_dif)
  
  

### diversity
load("v4_diversity_stanfit_test.RData")
draws_diversity <- rstan::extract(stanfit, pars = c("b_dif_rg"))


b_dif <- as.matrix(stanfit, pars = "b_dif_rg")
color_scheme_set("red")
mcmc_intervals(b_dif)



### total abund
load("v4_ta_stanfit.RData")
draws_ta <- rstan::extract(stanfit, pars = c("b_dif_rg"))


b_dif <- as.matrix(stanfit, pars = "b_dif_rg")
color_scheme_set("yellow")
mcmc_intervals(b_dif)



  