getScalingParams <- function(data) {
  # Get scaling params for the given data.
  # Assumes scaled data.
  scalingParams <- data %>%
    summarise(across(c(gdpGrowth, avrg_age, ftotinc, educ_attain_2.0_freq, race_1_freq, sex_1_freq),
                     list(mean = mean, sd = sd)))
  
  return (scalingParams)
}

scaleDataByParams <- function(data, params) {
  # Scale the given data by the given scaling params.
  scaledData <- data %>%
    mutate(
      gdpGrowth = (gdpGrowth - params$gdpGrowth_mean) / params$gdpGrowth_sd,
      avrg_age = (avrg_age - params$avrg_age_mean) / params$avrg_age_sd,
      ftotinc = (ftotinc - params$ftotinc_mean) / params$ftotinc_sd,
      educ_attain_2.0_freq = (educ_attain_2.0_freq - params$educ_attain_2.0_freq_mean) / params$educ_attain_2.0_freq_sd,
      race_1_freq = (race_1_freq - params$race_1_freq_mean) / params$race_1_freq_sd,
      sex_1_freq = (sex_1_freq - params$sex_1_freq_mean) / params$sex_1_freq_sd
    )
  
  return(scaledData)
}