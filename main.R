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
    prior(normal(0, 50), class = "b"),  # Weakly informative priors for coefficients
    prior(normal(0.5, 0.3), class = "Intercept"),  # Weakly informative prior for the intercept
    prior(cauchy(0, 0.1), class = "sigma")
  )

state_model <- lm(
  linearFormula,
  data = linearModelData
)

summary(state_model)


mcmc_state_model <- brm(
  linearFormula,
  data = linearModelData,
  family = gaussian(),  # Linear model assumes Gaussian errors
  prior = priors,
  chains = 5,  # Number of MCMC chains
  iter = 6000,  # Number of iterations per chain
  warmup = 2000,  # Burn-in period
  cores = 10  # Number of cores for parallel computation
)

summary(mcmc_state_model)
plot(mcmc_state_model)

# Posterior predictive checks
pp_check(mcmc_state_model)


# _______________________________________ 
# |                                     | 
# |     Model 2 - Hierarchical          | 
# |                                     | 
# _______________________________________ 