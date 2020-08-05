
# ----------
# Libraries
# ----------

library(dplyr)
library(tidyr)
library(ggplot2)
library(MARSS)
library(rstanarm)

# ----------
# CSVs
# ----------

ap <- read.csv("train18_polls.csv", stringsAsFactors = FALSE)
aa <- read.csv("train18_actual.csv", stringsAsFactors = FALSE)
tf <- read.csv("trump_favorable_18.csv", stringsAsFactors = FALSE)
s6 <- read.csv("sc-est2018-alldata6.csv", stringsAsFactors = FALSE)
sf <- read.csv("state_fips.csv", stringsAsFactors = FALSE)
ae <- read.csv("acs_edu_18.csv", stringsAsFactors = FALSE)
cp <- read.csv("congress18_polls.csv", stringsAsFactors = FALSE)

has6 <- which(as.vector(table(ap$index)) > 5)

# ----------
# Functions
# ----------

formatDates <- function(date, y) {
  s = strsplit(date, " - ")
  d1 = paste0(s[[1]][1], "/" ,y)
  d2 = paste0(s[[1]][2], "/" ,y)
  d1a = as.Date(d1, format = "%m/%d/%Y")
  d2a = as.Date(d2, format = "%m/%d/%Y")
  new_date = d1a + floor((d2a - d1a)/2)
  new_date = as.Date(new_date, origin = "1970-01-01")
  return(new_date)
}

# ----------
# Prep Model
# ----------

# import/format generic ballot polls:

congress18_polls <- cp %>%
  mutate(other = 100 - dem - gop) %>%
  filter(other > 0) %>%
  rowwise() %>%
  mutate(Date = formatDates(as.character(Date), as.character(Year))) %>%
  ungroup() %>%
  group_by(Date) %>%
  summarise(dem = mean(dem), gop = mean(gop), other = mean(other)) %>%
  mutate(d = log(dem/other)) %>%
  mutate(r = log(gop/other)) %>%
  select(Date, d, r)

date_range <- as.Date(range(congress18_polls$Date)[1]:range(congress18_polls$Date)[2], 
                      origin = "1970-01-01")

nat_polls_full <- data.frame(Date = date_range, index = 1:length(date_range)) %>%
  left_join(., congress18_polls, by = "Date") %>%
  mutate(d = as.numeric(as.character(d))) %>%
  mutate(r = as.numeric(as.character(r))) 

t_npf <- t(nat_polls_full[,3:ncol(nat_polls_full)])


# fit the SSM:

Q.model <- matrix(c("q1", "q2", "q2", "q3"), nrow = 2, ncol = 2)
R.model <- matrix(c("r1", "r2", "r2", "r3"), nrow = 2, ncol = 2)

mod.list <- list(B = diag(2), 
                 U = matrix(c(0,0), nrow = 2, ncol = 1), 
                 Q = Q.model,
                 Z = diag(2),
                 A = matrix(c(0,0), nrow = 2, ncol = 1), 
                 R = R.model)

congress18_fit <- MARSS(t_npf, model = mod.list, control = list(maxit = 5000))

delta <- congress18_fit$states[1,]
rho <- congress18_fit$states[2,]
o <- 100/(exp(delta) + exp(rho) + 1)
d <- o * exp(delta)
r <- o * exp(rho)
index <- 1:length(o)

congress18_fit_df <- data.frame(Date = date_range, index, delta, rho, d, r, o)

# import/format/regress state polls:

train18_polls <- ap %>%
  mutate(other = 100 - dem - gop) %>%
  filter(other > 0) %>%
  rowwise() %>%
  mutate(Date = formatDates(as.character(Date), as.character(Year))) %>%
  ungroup() 

state_regressions <- data.frame(index = c(), d_intent = c(), r_intent = c(), o_intent = c())

for (i in c(1:length(table(train18_polls$index)))) {
  state_polls <- train18_polls %>%
    filter(index == i) %>%
    filter(Date >= as.Date(min(congress18_fit_df$Date))) %>%
    #filter(Date <= as.Date(max(congress18_fit_df$Date))) %>%
    mutate(d_state = dem) %>%
    mutate(r_state = gop) %>%
    mutate(o_state = other) %>%
    select(Date, d_state, r_state, o_state)
  
  state_regress_df <- congress18_fit_df %>%
    select(Date, d, r, o) %>%
    left_join(state_polls, ., by = "Date") %>%
    filter(is.na(d_state) == FALSE)
  
  Md <- as.numeric(lm(d_state ~ offset(d), data = state_regress_df)$coefficients[1])
  Mr <- as.numeric(lm(r_state ~ offset(r), data = state_regress_df)$coefficients[1])
  Mo <- as.numeric(lm(o_state ~ offset(o), data = state_regress_df)$coefficients[1])
  
  d_intent = congress18_fit_df$d[nrow(congress18_fit_df)] + Md
  r_intent = congress18_fit_df$r[nrow(congress18_fit_df)] + Mr
  o_intent = congress18_fit_df$o[nrow(congress18_fit_df)] + Mo
  
  new_row <- data.frame(index = i, d_intent, r_intent, o_intent)
  state_regressions <- rbind(state_regressions, new_row)
}

state_regressions <- state_regressions %>%
  filter(index %in% has6)

# build training set:

break_spreads <- aa %>%
  left_join(., state_regressions, by = "index") %>%
  filter(is.na(d_intent) == FALSE) %>%
  mutate(third = 100 - dem - gop) %>%
  mutate(d_break = (dem - d_intent)/(o_intent - third)) %>%
  mutate(r_break = (gop - r_intent)/(o_intent - third)) %>%
  mutate(break_spread = d_break - r_break) %>%
  mutate(dem_current = ifelse(current_party == "dem", 1, 0)) %>%
  select(index, state, break_spread, dem_current)

trump_favorable <- tf %>%
  mutate(trump_favorable_spread = trump_favorable - trump_unfavorable) %>%
  select(state, trump_favorable_spread)

state_abbr <- sf

edu_data <- ae %>%
  select(state, hs_pct, white_college_pct)

total_pop <- s6 %>%
  left_join(., state_abbr, by = "NAME") %>%
  select(-NAME, -state_fips) %>%
  filter(ORIGIN == 0) %>%
  filter(SEX == 0) %>%
  group_by(state) %>%
  summarise(total_pop = sum(POPESTIMATE2018))

white_pop <- s6 %>%
  left_join(., state_abbr, by = "NAME") %>%
  select(-NAME, -state_fips) %>%
  filter(ORIGIN == 1) %>%
  filter(RACE == 1) %>%
  filter(SEX == 0) %>%
  group_by(state) %>%
  summarise(white_pop = sum(POPESTIMATE2018))

demographics_data <- total_pop %>%
  left_join(., white_pop, by = "state") %>%
  left_join(., edu_data, by = "state") %>%
  mutate(white_pct = 100*white_pop/total_pop) %>%
  mutate(weighted_white_college = white_pct * white_college_pct / 100) %>%
  select(state, white_pct, weighted_white_college, hs_pct)

train18_data <- break_spreads %>%
  left_join(., demographics_data, by = "state") %>%
  left_join(., trump_favorable, by = "state")

# ----------
# TRAIN MODEL
# ----------

form <- as.formula(break_spread ~ weighted_white_college + hs_pct + white_pct + dem_current + trump_favorable_spread)

bayes_model <- stan_glm(formula = form, 
                        data = train18_data, 
                        family = gaussian(link = "identity"), 
                        prior = lasso(df = 1, location = 0, scale = NULL, autoscale = TRUE),
                        refresh = 0,
                        adapt_delta = .9999,
                        seed = 12345)

bayes_model$coefficients
