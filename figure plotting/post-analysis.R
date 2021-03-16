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


d <- read.csv("whole_dataset_over40_ND.csv")
d_obs <- read.csv("observer_dataset_over40_ND.csv")
n_distinct(d$SpeciesRegion)
d$Species <- as.integer(as.factor(d$BBL))

## counts of species per region
summ_r <- d %>% group_by(BBL) %>% summarize(nreg = n_distinct(Region))


## counts of region for each species
summ_s <- d %>% group_by(Region) %>% summarize(nsp = n_distinct(BBL))

## create space and time indicators
d$space <- d$space.time
d$space[which(d$space == 1)] <- 0
d$space[which(d$space == 2)] <- 1

d$time <- d$space.time
d$time[which(d$time == 2)] <- 0
d$time[which(d$time == 1)] <- 1


### create an indicator ragged array that determines which species are present at which regions
# and which regions a species is found in, in both east and west (for a priori calculations)
# it is nreg x nspecies wide
sp_reg_mat <- as.matrix(read.csv("sp_reg_mat_nd.csv", header = F))
sp_reg_mat_east <- as.matrix(read.csv("sp_reg_mat_east.csv", header = F))
sp_reg_mat_west <- as.matrix(read.csv("sp_reg_mat_west.csv", header = F))

reg_sp_mat <- as.matrix(read.csv("reg_sp_mat_nd.csv", header = F))


# create a 0-1 indicator array for species in certain regions
sp_ind <- d %>% group_by(Region, Species) %>% select(Region, Species)
sp_ind <- sp_ind[!duplicated(sp_ind),]
sp_ind$id <- 1

sp_ind_wide <- spread(sp_ind, key = "Region", value = "id", fill = 0)
sp_ind_wide <- data.frame(sp_ind_wide) %>% select(-Species)








## 2nd stage - a priori calculation of space vs. time slopes --------------
load("v3_species_abundance_nd.RData")
draws <- rstan::extract(stanfit, pars = c("a", "b_space", "b_time", "B_TIME", "B_SPACE"))
save(draws, file = "species_abund_draws.RData")

b_space <- summary(stanfit, pars = "b_space")
b_time <- summary(stanfit, pars = "b_time")
a <- summary(stanfit, pars = "a")
b_dif <- 







#########################
#   SUMMARIZING MEANS  #
#######################


## get the mean b_space and b_time per species -----------------------------

# initialize
sp_index <- d %>% group_by(Species, BBL) %>% distinct(BBL)
sp_index <- sp_index %>% arrange(Species)

mean_space_sp <- vector()
mean_time_sp <- vector()
sp <- vector()

for( s in 1:42 ) {
  mean_space_sp[s] <- mean(draws$b_space[ , s, sp_reg_mat[s, 1:summ_r$nreg[s]]])
  mean_time_sp[s] <- mean(draws$b_time[, s, sp_reg_mat[s, 1:summ_r$nreg[s]]])
  
  sp[s] <- sp_index$BBL[s]
  
  
  
}

b_space_sp <- data.frame(mean_space_sp, sp)
b_time_sp <- data.frame(mean_time_sp, sp)


## repeat for regions
mean_space_reg <- vector()
mean_time_reg <- vector()

for( r in 1:32 ) {
  mean_space_reg[r] <- mean(draws$b_space[ , reg_sp_mat[r, 1:summ_s$nsp[r]], r])
  mean_time_reg[r] <- mean(draws$b_time[, reg_sp_mat[r, 1:summ_s$nsp[r]], r])
  
  reg <- seq(1:32)
  
  
  
}

b_space_reg <- data.frame(mean_space_reg, reg)
b_time_reg <- data.frame(mean_time_reg, reg)


beta_sp <- data.frame(b_space_sp, b_time_sp)
beta_reg <- data.frame(b_space_reg, b_time_reg)

## species plot
f_sp <- ggplot(beta_sp, mapping = aes(b_space_sp$mean_space_sp, b_time_sp$mean_time_sp)) + 
  geom_point(
    colour = "#00798C",
    alpha = 0.5,
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

f_sp <- f_sp + theme(legend.position = "none")
f_sp


## region plot
f_reg <- ggplot(beta_reg, mapping = aes(b_space_reg$mean_space_reg, b_time_reg$mean_time_reg)) + 
  geom_point(
    colour = "#00798C",
    alpha = 0.5,
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

f_reg <- f_reg + theme(legend.position = "none")
f_reg


f_all <- ggarrange(f_sp, f_reg,
                 labels = c("across species", "across regions",
                 ncol = 2, nrow = 1))
f_all


ggsave("species_abundance.png", device = "png", plot = f_all,
       width = 50, height = 20, units = "cm")



save(b_space_sp, b_time_sp, b_space_reg, b_time_reg, file = "species_abund_means.RData")







#########################
#      EAST REGION     #
#######################


## summarize across the 2 biomes ---------------------------------------------
biome <- read.csv("forest_biome_input.csv")

# how many species and regions in eastern forests?
east_forest <- biome %>% filter(biome == 1)

d_east <- d %>% filter(Region %in% east_forest$region)

reg_east <- d_east %>% distinct(Region)
nregions_east <- reg_east %>% n_distinct(reg_east$Region)

species_east <- d_east %>% group_by(Species, BBL) %>% distinct(Species) %>% arrange(Species)
nspecies_east <- species_east %>% n_distinct(species_east$Species)


## counts of species per region
summ_r_east <- d_east %>% group_by(BBL) %>% summarize(nreg = n_distinct(Region))


## counts of region for each species
summ_s_east <- d_east %>% group_by(Region) %>% summarize(nsp = n_distinct(BBL))


### SPECIES -- EAST ------------------------------------------------------------
mean_east_space_sp <- vector()
mean_east_time_sp <- vector()
sp <- vector()

for( s in 1:42 ) {
  
  if ( s %in% species_east$Species ) {
  species <- species_east %>% filter(Species == s)
  summ_r_east_filter <- summ_r_east %>% filter(BBL == species$BBL)

  
  mean_east_space_sp[s] <- mean(draws$b_space[ , s, sp_reg_mat[s, 1:summ_r_east_filter$nreg]])
  mean_east_time_sp[s] <- mean(draws$b_time[, s, sp_reg_mat[s, 1:summ_r_east_filter$nreg]])
  
  sp[s] <- species$BBL
  
  }else{
  
}

}
b_east_space_sp <- data.frame(na.omit(mean_east_space_sp), na.omit(sp))
b_east_time_sp <- data.frame(na.omit(mean_east_time_sp), na.omit(sp))
b_east_sp <- data.frame(b_east_space_sp, b_east_time_sp)


## REGIONS -- EAST --------------------------------------------------------------
mean_east_space_reg <- vector()
mean_east_time_reg <- vector()
reg <- vector()

for( r in 1:32 ) {
  
  if ( r %in% reg_east$Region ) {
    summ_s_east_filter <- summ_s_east %>% filter(Region == r)
    mean_east_space_reg[r] <- mean(draws$b_space[ , reg_sp_mat[r, 1:summ_s_east_filter$nsp], r])
    mean_east_time_reg[r] <- mean(draws$b_time[, reg_sp_mat[r, 1:summ_s_east_filter$nsp], r])
    
    reg[r] <- r
    
  }else{
    
    
    
  }
}

b_east_space_reg <- data.frame(na.omit(mean_east_space_reg), na.omit(reg))
b_east_time_reg <- data.frame(na.omit(mean_east_time_reg), na.omit(reg))
b_east_reg <- data.frame(b_east_space_reg, b_east_time_reg)

## species plot
f_east_sp <- ggplot(b_east_sp, mapping = aes(na.omit.mean_east_space_sp.,  na.omit.mean_east_time_sp.)) + 
  geom_point(
    colour = "#00798C",
    alpha = 0.5,
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

f_east_sp <- f_east_sp + theme(legend.position = "none")
f_east_sp


## region plot
f_east_reg <- ggplot(b_east_reg, mapping = aes(na.omit.mean_east_space_reg., na.omit.mean_east_time_reg.)) + 
  geom_point(
    colour = "#00798C",
    alpha = 0.5,
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

f_east_reg <- f_east_reg + theme(legend.position = "none")
f_east_reg


f_east_all <- ggarrange(f_east_sp, f_east_reg,
                        labels = c("across species", "across regions",
                                   ncol = 2, nrow = 1))
f_east_all



save(b_east_space_sp, b_east_time_sp, b_east_space_reg, b_east_time_reg, file = "species_abund_means_EAST.RData")




#########################
#      WEST REGION     #
#######################
west_forest <- biome %>% filter(biome == 2)

d_west <- d %>% filter(Region %in% west_forest$region)

reg_west <- d_west %>% distinct(Region)
nregions_west <- reg_west %>% n_distinct(reg_west$Region)

species_west <- d_west %>% group_by(Species, BBL) %>% distinct(Species) %>% arrange(Species)
nspecies_west <- species_west %>% n_distinct(species_west$Species)


## counts of species per region
summ_r_west <- d_west %>% group_by(BBL) %>% summarize(nreg = n_distinct(Region))


## counts of region for each species
summ_s_west <- d_west %>% group_by(Region) %>% summarize(nsp = n_distinct(BBL))

### SPECIES -- west ------------------------------------------------------------
mean_west_space_sp <- vector()
mean_west_time_sp <- vector()
sp <- vector()

for( s in 1:42 ) {
  
  if ( s %in% species_west$Species ) {
    species <- species_west %>% filter(Species == s)
    summ_r_west_filter <- summ_r_west %>% filter(BBL == species$BBL)
    
    
    mean_west_space_sp[s] <- mean(draws$b_space[ , s, sp_reg_mat[s, 1:summ_r_west_filter$nreg]])
    mean_west_time_sp[s] <- mean(draws$b_time[, s, sp_reg_mat[s, 1:summ_r_west_filter$nreg]])
    
    sp[s] <- species$BBL
    
  }else{
    
  }
  
}
b_west_space_sp <- data.frame(na.omit(mean_west_space_sp), na.omit(sp))
b_west_time_sp <- data.frame(na.omit(mean_west_time_sp), na.omit(sp))
b_west_sp <- data.frame(b_west_space_sp, b_west_time_sp)


## REGIONS -- west --------------------------------------------------------------
mean_west_space_reg <- vector()
mean_west_time_reg <- vector()
reg <- vector()

for( r in 1:32 ) {
  
  if ( r %in% reg_west$Region ) {
  summ_s_west_filter <- summ_s_west %>% filter(Region == r)
  mean_west_space_reg[r] <- mean(draws$b_space[ , reg_sp_mat[r, 1:summ_s_west_filter$nsp], r])
  mean_west_time_reg[r] <- mean(draws$b_time[, reg_sp_mat[r, 1:summ_s_west_filter$nsp], r])
  
  reg[r] <- r
  
  }else{
    
    
    
  }
}

b_west_space_reg <- data.frame(na.omit(mean_west_space_reg), na.omit(reg))
b_west_time_reg <- data.frame(na.omit(mean_west_time_reg), na.omit(reg))
b_west_reg <- data.frame(b_west_space_reg, b_west_time_reg)


## species plot
f_west_sp <- ggplot(b_west_sp, mapping = aes(na.omit.mean_west_space_sp., na.omit.mean_west_time_sp.)) + 
  geom_point(
    colour = "#00798C",
    alpha = 0.5,
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

f_west_sp <- f_west_sp + theme(legend.position = "none")
f_west_sp


## region plot
f_west_reg <- ggplot(b_west_reg, mapping = aes(na.omit.mean_west_space_reg., na.omit.mean_west_time_reg.)) + 
  geom_point(
    colour = "#00798C",
    alpha = 0.5,
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

f_west_reg <- f_west_reg + theme(legend.position = "none")
f_west_reg


f_both <- ggarrange(f_east_sp, f_east_reg, f_west_sp, f_west_reg,
                    labels = c("Eastern Forests: across species", "Eastern Forests: across regions",
                               "Northwestern Mountains: across species", "Northwestern Mountains: across regions",
                               ncol = 2, nrow = 2))
f_both

ggsave("species_abundance_ECOREGIONS.png", device = "png", plot = f_both,
       width = 40, height = 30, units = "cm")




b_west_space_reg <- data.frame(mean_west_space_reg, reg)
b_west_time_reg <- data.frame(mean_west_time_reg, reg)


save(b_west_space_sp, b_west_time_sp, b_west_space_reg, b_west_time_reg, file = "species_abund_means_WEST.RData")








#######################################
#   SLOPE / CORRELATION COEFFICIENT  #
#####################################


## FUNCTIONS
# correlation coefficient
function (y,x){
  
  n = length(x)
  sy = sum(y)
  sx = sum(x)
  ssx = sum(x^2)
  sxy = sum(y*x)
  
  xsq = x^2
  sxsq = sx^2
  
  ysq = y^2
  sysq = sy^2
  
  r = (n*sxy - (sx * sy)) / sqrt((n*sum(xsq) - sxsq) * (n*sum(ysq) - sysq))
  
}

# equivalent to lm()
bsl = function(y,x){
  n = length(x)
  sy = sum(y)
  sx = sum(x)
  ssx = sum(x^2)
  sxy = sum(y*x)
  b = (n*sxy - sx*sy)/(n*ssx - sx^2)
  return(b)
}



# --------------------------------------------------------------


## SET UP
nspecies = 42
nregions = 32
niterations = 2000
N = 300

# estimate of the slope across all species in a given region
for(r in 1:nregions){
  pred_data = data.frame(b_space = seq(min(b_space[, 1:summ_s[r], r]), max(b_space[, 1:summ_s[r], r]), length = N))
  
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, reg_sp_mat[r, 1:summ_s[r]], r] ~ b_space[i, reg_sp_mat[r, 1:summ_s[r]], r])
    
    intercept_by_r[i,r] = mlm$coefficients[[1]]
    slope_by_r[i,r] = mlm$coefficients[[2]]
    
    pred_lines[i, r, 1:N] <- predict(mlm, newdata = pred_data)
    
  }
  
  # summarize across all iterations
  mean_intercept_by_r[r] = mean(intercept_by_r[,r])
  
  mean_slope_by_r[r] = mean(slope_by_r[,r])
  
  
}



# estimate of the slope across regions for a given species
for(s in 1:nspecies){
  pred_data = data.frame(b_space = seq(min(b_space[, 1:summ_r[s], s]), max(b_space[, 1:summ_r[s], s]), length = N))
  
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, s, sp_reg_mat[s, 1:summ_r[s]]] ~ b_space[i, s, sp_reg_mat[s, 1:summ_r[s]]])
    
    intercept_by_s[i, s] = mlm$coefficients[[1]]
    slope_by_s[i, s] = mlm$coefficients[[2]]
    
    pred_lines[i, s, 1:N] <- predict(mlm, newdata = pred_data)
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_s[s] = mean(intercept_by_s[,s])
  
  mean_slope_by_s[s] = mean(slope_by_s[,s])
  
  
}



# EASTERN FOREST -----------------------------------------------------------------
for(r in reg_east){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, reg_sp_mat[r,1:summ_s[r]], r] ~ b_space[i, reg_sp_mat[r, 1:summ_s[r]], r])
    
    intercept_by_r_east[i,r] = mlm$coefficients[[1]]
    slope_by_r_east[i,r] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_r_east[r] = mean(intercept_by_r_east[,r])
  
  mean_slope_by_r_east[r] = mean(slope_by_r_east[,r])
  
  
}


# estimate of the slope across regions for a given species
for(s in species_east){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, s, sp_reg_mat_east[s, 1:summ_r[s]]] ~ b_space[i, s, sp_reg_mat_east[s, 1:summ_r[s]]])
    
    intercept_by_s_east[i, s] = mlm$coefficients[[1]]
    slope_by_s_east[i, s] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_s[s] = mean(intercept_by_s_east[,s])
  
  mean_slope_by_s[s] = mean(slope_by_s_east[,s])
  
  
}



# NORTHWESTERN MOUNTAINS ------------------------------------------------------------

# estimate of the slope across all species in a given region
for(r in reg_west){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, reg_sp_mat[r,1:summ_s[r]], r] ~ b_space[i, reg_sp_mat[r, 1:summs[r]], r])
    
    intercept_by_r_west[i,r] = mlm$coefficients[[1]]
    slope_by_r_west[i,r] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_r_west[r] = mean(intercept_by_r_west[,r])
  
  mean_slope_by_r_west[r] = mean(slope_by_r_west[,r])
  
  
}


species_west <- d_west %>% distinct(Species)

nspecies_west <- species_west %>% n_distinct(Species)

# estimate of the slope across regions for a given species
for(s in species_east){
  for (i in 1:niterations){
    
    mlm = lm(b_time[i, s, sp_reg_mat_west[s, 1:summ_r[s]]] ~ b_space[i, s, sp_reg_mat_west[s, 1:summ_r[s]]])
    
    intercept_by_r_west[i, s] = mlm$coefficients[[1]]
    slope_by_r_west[i, s] = mlm$coefficients[[2]]
    
    
  }
  
  # summarize across all iterations
  mean_intercept_by_s_west[s] = mean(intercept_by_s_west[,s])
  
  mean_slope_by_s_west[s] = mean(slope_by_s_west[,s])
  
  
}



