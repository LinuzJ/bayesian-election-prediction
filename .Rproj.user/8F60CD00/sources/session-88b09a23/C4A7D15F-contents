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

# _______________________________________ 
# |                                     | 
# |       Data Transformation           | 
# |                                     | 
# _______________________________________ 

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

# Use only 2000 ->
votingData <- votingData %>% 
  filter(year >= 2000)

votingData <- votingData[,c("year", "state_fips", "candidatevotes", "totalvotes", "party_simplified")]

# Other parties -> unique group 
otherParties <- votingData %>%
  filter(party_simplified != "REPUBLICAN" & party_simplified != "DEMOCRAT") %>%
  group_by(year, state_fips) %>%
  summarize(candidatevotes = sum(candidatevotes, na.rm = TRUE), .groups = "drop")

# Get yearly Total per State
yearlyTotalVotes <- votingData %>%
  group_by(year, state_fips) %>%
  summarize(totalvotes = first(totalvotes), .groups = "drop")
# Include above in otherParties
otherParties <- otherParties %>%
  left_join(
    yearlyTotalVotes,
    by = c("year", "state_fips")
  ) %>%
  mutate(party_simplified = "OTHER")

# Append to total votingData
votingData <- bind_rows(votingData, otherParties)

# _______________________________________ 
# |                                     | 
# |       Model Building                | 
# |                                     | 
# _______________________________________ 
