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
if (!require(bayesplot)) {
  install.packages("bayesplot")
  library(bayesplot)
}
cmdstan_installed <- function(){
  res <- try(out <- cmdstanr::cmdstan_path(), silent = TRUE)
  !inherits(res, "try-error")
}
if(!cmdstan_installed()){
  install_cmdstan()
}

source("data_processing.R")
source("scaling.R")

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

linearVariables <- c("state_id", "year", "vote_fraction", "gdpGrowth", "avrg_age", "ftotinc", "educ_attain_2.0_freq", "race_1_freq", "totalvotes", "sex_1_freq")

linearModelData <- totalData %>%
  filter(party_simplified == "REPUBLICAN") %>%
  mutate(across(linearVariables, as.numeric)) %>%
  select(linearVariables) %>%
  drop_na(gdpGrowth, avrg_age, ftotinc, educ_attain_2.0_freq, race_1_freq) %>%
  mutate(
    gdpGrowth = scale(gdpGrowth),
    avrg_age = scale(avrg_age),
    ftotinc = scale(ftotinc),
    educ_attain_2.0_freq = scale(educ_attain_2.0_freq),
    sex_1_freq = scale(sex_1_freq),
    race_1_freq = scale(race_1_freq)
  )

hierarchicalVariables <- c("state_id", "year", "vote_fraction", "gdpGrowth", "avrg_age", "ftotinc", "educ_attain_2.0_freq", "race_1_freq", "totalvotes", "sex_1_freq")

hierarchicalModelData <- totalData %>%
  filter(party_simplified == "REPUBLICAN") %>%
  mutate(across(hierarchicalVariables, as.numeric)) %>%
  select(hierarchicalVariables) %>%
  drop_na(gdpGrowth, avrg_age, ftotinc, educ_attain_2.0_freq, race_1_freq) %>%
  mutate(
    gdpGrowth = scale(gdpGrowth),
    avrg_age = scale(avrg_age),
    ftotinc = scale(ftotinc),
    educ_attain_2.0_freq = scale(educ_attain_2.0_freq),
    sex_1_freq = scale(sex_1_freq),
    race_1_freq = scale(race_1_freq)
  )

hierarchicalScalingParams <- getScalingParams(hierarchicalModelData)

hist(hierarchicalModelData$vote_fraction, 
     main = "Histogram of vote_fraction", 
     xlab = "% of Republican Votes", 
     ylab = "Frequency", 
     col = "red", 
     border = "black",
     breaks = 20,
     xlim = c(0, 1)
)

# _______________________________________ 
# |                                     | 
# |     Model 1 - Linear Beta           | 
# |                                     | 
# _______________________________________ 

# --------- Formula --------- 
linearFormula <- bf(vote_fraction ~ gdpGrowth + avrg_age + ftotinc + educ_attain_2.0_freq + sex_1_freq + race_1_freq)

# --------- Priors --------- 
priors <- c(
    prior(normal(0, 10), class = "b"),
    prior(normal(0.5, 1), class = "Intercept"),
    prior(cauchy(0, 5), class = "phi") 
  )

# --------- MCMC --------- 
linearBetaModel <- brm(
  linearFormula,
  data = linearModelData,
  family = Beta(),
  prior = priors,
  chains = 4,
  iter = 4000,
  warmup = 1000,
  cores = 6
)

plot(linearBetaModel)


# _______________________________________ 
# |                                     | 
# |     Model 1 - Linear Gaussian       | 
# |                                     | 
# _______________________________________ 

# --------- Formula --------- 
linearFormula <- bf(vote_fraction ~ gdpGrowth + avrg_age + ftotinc + educ_attain_2.0_freq + sex_1_freq + race_1_freq)

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


# --------- MCMC --------- 
linearGaussianModel <- brm(
  linearFormula,
  data = linearModelData,
  family = gaussian(),
  prior = priors,
  chains = 5,
  iter = 10000,
  warmup = 3000,
  cores = 6
)

plot(linearGaussianModel)


# _______________________________________ 
# |                                     | 
# |     Model 2 - Hierarchical          | 
# |                                     | 
# _______________________________________ 


hierarchicalFormula <- bf(
  vote_fraction ~ gdpGrowth + avrg_age + ftotinc + educ_attain_2.0_freq + race_1_freq + sex_1_freq +
    (1 | state_id) + (1 | year)
)

hierarchicalPrior <- c(
  prior(normal(0, 5), class = "b"),
  prior(normal(0.5, 1), class = "Intercept"),
  prior(lognormal(1, 0.5), class = "phi"),
  prior(normal(0, 1), class = "sd")
)

hierarchicalModel <- brm(
  formula = hierarchicalFormula,
  prior = hierarchicalPrior,
  data = hierarchicalModelData,
  family = Beta(),
  chains = 8,
  iter = 10000,
  warmup = 5000,
  cores = 6,
  control = list(adapt_delta = 0.99, max_treedepth = 16)  
)

plot(hierarchicalModel)


# _______________________________________ 
# |                                     | 
# |       Model Analysis                | 
# |                                     | 
# _______________________________________ 

# _______________________________________
# |                                     |
# |       Convergence Diagnostics       |
# |                                     |
# _______________________________________

summary(linearGaussianModel)
summary(linearBetaModel)
summary(hierarchicalModel)



# _______________________________________ 
# |                                     | 
# |   Posterior Predictive Checks       | 
# |                                     | 
# _______________________________________ 


pp_check(linearGaussianModel)
pp_check(linearBetaModel)
pp_check(hierarchicalModel)


# _______________________________________ 
# |                                     | 
# |         Model Comparison            | 
# |                                     | 
# _______________________________________ 

loo_linear_beta_results <- loo(linearBetaModel)
loo_linear_gaussian_results <- loo(linearGaussianModel)
loo_hierarchical_results <- loo(hierarchicalModel)


comparison <- loo_compare(loo_linear_beta_results, loo_hierarchical_results, loo_linear_gaussian_results)
print(comparison)


# _______________________________________ 
# |                                     | 
# |         Predictive Accuracy         | 
# |                                     | 
# _______________________________________ 

