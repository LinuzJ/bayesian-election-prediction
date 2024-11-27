library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

if (!require(brms)) {
  install.packages("brms")
  library(brms)
}

if(!require(cmdstanr)){
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  library(cmdstanr)
}

cmdstan_installed <- function(){
  res <- try(out <- cmdstanr::cmdstan_path(), silent = TRUE)
  !inherits(res, "try-error")
}
if(!cmdstan_installed()){
  install_cmdstan()
}

source("data_processing.R")

# _______________________________________ 
# |                                     | 
# |       Data Transformation           | 
# |                                     | 
# _______________________________________ 

# --------- Read and Parse Census Data --------- 
censusData <- read.csv("data/census-data.csv")
censusDataStateLevel <- processCensusData(censusData)

# --------- Read and Parse Voting Data --------- 
votingData <- read.csv("data/voting-data.csv")
votingData <- processVotingData(votingData)

# --------- Read and Parse GDP Data --------- 
gdpData <- read.csv("data/gdp-data.csv")
gdpData <- processGdpData(gdpData)


# --------- Combine Data --------- 
votingData <- votingData %>%
  mutate(state_id = as.integer(state_id))

censusDataStateLevel <- censusDataStateLevel %>%
  mutate(state_id = as.integer(state_id))

gdpData <- gdpData %>%
  mutate(state_id = as.integer(state_id))

totalData <- votingData %>%
  left_join(censusDataStateLevel, by = c("year", "state_id")) %>%
  left_join(gdpData, by = c("year", "state_id"))

# _______________________________________ 
# |                                     | 
# |       Model Building                | 
# |                                     | 
# _______________________________________ 

# _______________________________________ 
# |                                     | 
# |     Model 1 - Linear                | 
# |                                     | 
# _______________________________________ 

variables <- c("state_id", "year", "vote_fraction", "gdpGrowth", "avrg_age", "ftotinc", "educ_attain_2.0_freq", "race_1_freq", "totalvotes")

linearModelData <- totalData %>%
  filter(party_simplified == "REPUBLICAN") %>%
  mutate(across(variables, as.numeric)) %>%
  select(variables) %>%
  group_by(year, state_id)


# --------- Formula --------- 
linearFormula <- bf(vote_fraction ~ gdpGrowth + avrg_age + ftotinc + educ_attain_2.0_freq + race_1_freq)

# --------- Priors --------- 
priors <- c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0.5, 1), class = "Intercept"),
    prior(cauchy(0, 5), class = "sigma")
  )

state_model <- lm(
  linearFormula,
  data = linearModelData
)

summary(state_model)


mcmc_state_model <- brm(
  linearFormula,
  data = linearModelData,
  family = gaussian(),
  prior = priors,
  chains = 5,
  iter = 15000,
  warmup = 3000,
  cores = 5
)

summary(mcmc_state_model)
plot(mcmc_state_model)

pp_check(mcmc_state_model)


# _______________________________________ 
# |                                     | 
# |     Model 2 - Hierarchical          | 
# |                                     | 
# _______________________________________ 
hierarchicalVariables <- c("state_id", "year", "vote_fraction", "gdpGrowth", "avrg_age", "ftotinc", "educ_attain_2.0_freq", "race_1_freq", "totalvotes")
#hierarchicalVariables <- c("state_id", "year", "vote_fraction", "inctot", "avrg_age", "mortamt1", "sex_1_freq", "educ_attain_1.0_freq", "gdpGrowth",  "totalvotes")

hierarchicalModelData <- totalData %>%
  filter(party_simplified == "REPUBLICAN") %>%
  mutate(across(hierarchicalVariables, as.numeric)) %>%
  select(hierarchicalVariables) %>%
  group_by(year, state_id)

formula <- bf(
  vote_fraction ~ gdpGrowth + avrg_age + ftotinc + educ_attain_2.0_freq + race_1_freq +
    (1 | state_id) + (1 | year)
)

prior <- c(
  # Priors for fixed effects
  prior(normal(0, 10), class = "b"),
  prior(normal(0.5, 0.5), class = "Intercept"),
  prior(normal(0, 1), class = "sigma", lb = 0),
  prior(normal(0, 1), class = "sd")
)

model <- brm(
  formula = formula,
  prior = prior,
  data = hierarchicalModelData,
  family = gaussian(),
  chains = 5,
  iter = 10000,
  warmup = 2000,
  cores = 5,
  control = list(adapt_delta = 0.98, max_treedepth = 14)  
)
summary(model)
plot(model)
pp_check(model)
# _______________________________________ 
# |                                     | 
# |       Model Analysis                | 
# |                                     | 
# _______________________________________ 
# _______________________________________ 
# |                                     | 
# |     Model 1 - Linear                | 
# |                                     | 
# _______________________________________ 
loo_results <- loo(model)

print(loo_results)

