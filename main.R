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

totalData <- votingData %>%
  left_join(censusDataStateLevel, by = c("year", "state_id"))

# _______________________________________ 
# |                                     | 
# |       Model Building                | 
# |                                     | 
# _______________________________________ 
