processCensusData <- function(raw_data) {
  countyData <- select(raw_data,-c(state_po, county_name))
  
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
  return(censusDataStateLevel)
}

processVotingData <- function(raw_data) {
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
  
  # Add %ofVotes, change name of state_id
  votingData <- votingData %>% 
    mutate(
      vote_fraction = candidatevotes / totalvotes
    ) %>%
    mutate(
      state_id = state_fips
    ) %>%
    select(-state_fips)
  return (votingData)
}


processGdpData <- function(raw_data) {
  
  # Use only necessary cols, parse state_id
  gdpData <- raw_data %>%
    select(-c(GeoName, Region, TableName, LineCode, IndustryClassification, Description, Unit)) %>%
    mutate(GeoFIPS = as.character(GeoFIPS)) %>%
    mutate(state_id = str_sub(GeoFIPS, 2, 3)) %>%
    filter(state_id != "00") %>%
    select(-GeoFIPS) %>%
    slice(1:(n() - 4)) # Remove last 4 rows (NA rows)
  
  # remove X from col names
  colnames(gdpData) <- gsub("^X", "", colnames(gdpData))
  
  # replace manual string "(NA)" by the previous years value.
  gdpData <- gdpData %>%
    mutate(`2020` = ifelse(`2020` == "(NA)", `2019`, `2020`))

  # Make numeric
  gdpData <- gdpData %>%
    mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  # Remove non-state FIPS numbers (read more: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code#:~:text=codes%20by%20county-,Supplemental%20codes%20for%20maritime%20areas,-%5Bedit%5D)
  eas_codes <- c(57, 58, 59, 61, 65, 73, 75, 77, 91, 92, 93, 94, 95, 96, 97, 98)
  
  gdpData <- gdpData %>%
    filter(!state_id %in% eas_codes)
  
  
  # ------ GROUP BY STATE -------
  gdpData <- gdpData %>%
    group_by(state_id) %>%
    summarize(across(everything(), sum, na.rm = TRUE), .groups = "drop")
  
  # ------ TRANSFORM TO RUNNING TWO YEAR GROWTH -------
  gdpData <- gdpData %>%
    # Pivot two three columns, with state_id, year, gdp
    pivot_longer(-state_id, names_to = "year", values_to = "gdp") %>%
    # Sort by state_id, year in order to calclate running values
    arrange(state_id, year) %>%
    group_by(state_id) %>%
    # growth = (now - previous) / previous
    mutate(gdpGrowth = (gdp - lag(gdp, 2)) / lag(gdp, 2) * 100) %>%
    filter(!is.na(gdpGrowth)) %>%
    select(state_id, year, gdpGrowth)

  gdpData <- gdpData %>%
    mutate(year = as.integer(year))
    return (gdpData)
}
