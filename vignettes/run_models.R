library(dplyr)
library(ggplot2)
library(brms)
library(stringr)
library(readr)
library(reshape2)
library(patchwork)
library(influ2)
library(bayesplot)
library(nzsf)
library(viridis)

setwd("/home/darcy/Projects/length2age/vignettes")

do_run1 <- FALSE
do_run2 <- FALSE
do_run3 <- TRUE

theme_set(theme_bw())

options(mc.cores = 6)

criterion <- c("loo")
# criterion <- c("loo", "marglik")

# Load data ----

# load("data/Darcy_HAK.ALK.data.SOP.otoliths.rdata")
load("../data/Darcy_HAK.ALK.data.ALL.otoliths.rdata")

d <- data2 %>%
  ungroup() %>%
  # filter(fyear %in% 2000:2010) %>%
  mutate(age = ordered(floor(age)), cyear = fyear, fyear = as.factor(fyear), month = as.factor(month))

glimpse(d)
dim(d)

table(data2$age)
table(data2$area)
table(data2$fyear)
table(data2$fmonth)
table(data2$month)
table(data2$origin)

# In this version the idea is to provide counts of each length for an age, and these use these counts as weights in the model. Can speed things up. Will not work for spatial models.
d2 <- d %>%
  group_by(fyear, month, age, length) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(cyear = as.numeric(as.character(fyear)))

glimpse(d2)
nrow(d)
nrow(d2) # reduces data set by about half

# do a benchmark on these two runs

fast_0 <- brm(age ~ s(length, cyear, k = c(3, 26)) + month, data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
fast_1 <- brm(age | weights(count) ~ s(length, cyear, k = c(3, 26)) + month, data = d2, family = cumulative("logit"), control = list(adapt_delta = 0.99))

# Full models ----

if (do_run1) {
  # WHEN DOING THIS AGAIN SHOULD DO s(long, lat) and should really turn into equal area then spec the same k in x and y
  # Should try models with error in age and/or length e.g.: 
  # age | mi(age_sd) ~ s(length, k = 3)
  # age | mi(age_sd) ~ s(me(length, length_sd), k = 3)
  # age | mi(age_sd) ~ s(me(length, 1), k = 3)
  full_1c <- brm(age ~ s(length, cyear, k = c(3, 26)) + origin + month + s(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  full_2c <- brm(age ~ t2(length, cyear, k = c(3, 26)) + origin + month + t2(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  full_3c <- brm(age ~ s(length, cyear, k = c(3, 26)) + origin + month + t2(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  full_4c <- brm(age ~ t2(length, cyear, k = c(3, 26)) + origin + month + s(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))

  full_1c <- add_criterion(full_1c, criterion = criterion)
  full_2c <- add_criterion(full_2c, criterion = criterion)
  full_3c <- add_criterion(full_3c, criterion = criterion)
  full_4c <- add_criterion(full_4c, criterion = criterion)
  
  save(full_1c, full_2c, full_3c, full_4c, file = "full_ord.rda")
}

# Reduced models ----

if (do_run2) {
  fit_0c <- brm(age ~ length, data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))#, save_pars = save_pars(all = TRUE))
  fit_1c <- brm(age ~ s(length), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))#, save_pars = save_pars(all = TRUE))
  fit_2c <- brm(age ~ s(length, k = 3), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_3c <- brm(age ~ s(length, bs = "cr"), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_4c <- brm(age ~ t2(length), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_5c <- brm(age ~ t2(length, bs = "ts"), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_6c <- brm(age ~ s(length, k = 3, by = fyear), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_7c <- brm(age ~ s(length, cyear, k = c(3, 26)), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_8c <- brm(age ~ t2(length, cyear, k = c(3, 26)), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_9c <- brm(age ~ s(length, k = 3, by = fyear) + month + s(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_10c <- brm(age ~ s(length, k = 3, by = fyear) + month + t2(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_11c <- brm(age ~ t2(length, cyear, k = c(3, 26)) + month + t2(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_12c <- brm(age ~ t2(length, cyear, k = c(3, 26)) + t2(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  

  fit_0c <- add_criterion(fit_0c, criterion = criterion)
  fit_1c <- add_criterion(fit_1c, criterion = criterion)
  fit_2c <- add_criterion(fit_2c, criterion = criterion)
  fit_3c <- add_criterion(fit_3c, criterion = criterion)
  fit_4c <- add_criterion(fit_4c, criterion = criterion)
  fit_5c <- add_criterion(fit_5c, criterion = criterion)
  fit_6c <- add_criterion(fit_6c, criterion = criterion)
  fit_7c <- add_criterion(fit_7c, criterion = criterion)
  fit_8c <- add_criterion(fit_8c, criterion = criterion)
  fit_9c <- add_criterion(fit_9c, criterion = criterion)
  fit_10c <- add_criterion(fit_10c, criterion = criterion)
  fit_11c <- add_criterion(fit_11c, criterion = criterion)
  fit_12c <- add_criterion(fit_12c, criterion = criterion)

  save(fit_0c, fit_1c, fit_2c, fit_3c, fit_4c, fit_5c, 
       fit_6c, fit_7c, fit_8c, fit_9c, fit_10c, fit_11c, fit_12c,
       file = "fit_ord.rda")
}

# More models ----

if (do_run3) {
  fit_2cv <- brm(bf(age ~ s(length, k = 3)) + lf(disc ~ length), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_2r <- brm(age ~ s(length, k = 3), data = d, family = cratio("logit"), control = list(adapt_delta = 0.99))
  fit_2s <- brm(age ~ s(length, k = 3), data = d, family = sratio("logit"), control = list(adapt_delta = 0.99))
  # # fit_2cp <- brm(age ~ s(length, k = 3, by = fyear), data = d, family = cumulative("probit"), control = list(adapt_delta = 0.99))
  fit_13c <- brm(age ~ t2(length, cyear, k = c(3, 26)) + s(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_14c <- brm(age ~ t2(length, cyear, k = c(10, 26)) + s(lat, long), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  fit_15c <- brm(age ~ t2(length, cyear, k = c(3, 26)) + s(lat, long, bs = "gp"), data = d, family = cumulative("logit"), control = list(adapt_delta = 0.99))
  # do time as fyear + fmonth / 12
  # space as the clusters (could have sub clusters within bigger fisheries)
  
  fit_2cv <- add_criterion(fit_2cv, criterion = criterion)
  fit_2r <- add_criterion(fit_2r, criterion = criterion)
  fit_2s <- add_criterion(fit_2s, criterion = criterion)
  fit_13c <- add_criterion(fit_13c, criterion = criterion)
  fit_14c <- add_criterion(fit_14c, criterion = criterion)
  fit_15c <- add_criterion(fit_15c, criterion = criterion)
  
  save(fit_2cv, fit_2r, fit_2s,
       fit_13c, fit_14c, fit_15c,
       file = "fit_more.rda")
}
