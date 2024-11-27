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


# --------- Read and Parse Census Data --------- 
countyData <- read.csv("data/county_census_and_election_result.csv")
countyData <- select(countyData,-c(state_po, county_name))

# Split FIPS column to state and county separately
# new_column -> state_id 
countyData <- countyData %>%
  mutate(
    state_id = if_else(
      str_length(county_fips) > 2,
      str_sub(county_fips, 1, 2),
      as.character(county_fips)
    )
  ) %>%
  select(-county_fips)

# GroupBy year and state_id -> mean over counties
censusDataStateLevel <- countyData %>% 
  group_by(year, state_id) %>%
  summarize(across(everything(), mean, na.rm = TRUE), .groups = "drop")

# --------- Read and Parse Voting Data --------- 
votingData <- read.csv("data/1976-2020-president.csv")

                       