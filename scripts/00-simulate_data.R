#### Preamble ####
# Purpose: Simulates a dataset of Toronto beach observations with synthetic variables.
# Author: Yizhe Chen
# Date: 1 Dec 2024
# Contact: yz.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed
# Any other information needed? The simulation focuses on synthetic interactions between variables.


#### Workspace setup ####
library(tidyverse)
set.seed(853)


#### Simulate data ####
# Define the number of observations
n <- 1000

# Simulate the variables
simulated_data <- tibble(
  data_collection_date = sample(
    seq.Date(from = as.Date("2010-01-01"), to = as.Date("2024-01-01"), by = "day"),
    size = n,
    replace = TRUE
  ),
  beach_name = sample(
    c("Sunnyside Beach", "Woodbine Beach", "Cherry Beach", 
      "Hanlan's Point Beach", "Marie Curtis Park Beach", 
      "Kew Balmy Beach", "Centre Island Beach"),
    size = n,
    replace = TRUE
  ),
  wind_speed = round(rnorm(n, mean = 10, sd = 3), 1),
  air_temp = round(rnorm(n, mean = 20, sd = 5), 1),
  water_temp = round(rnorm(n, mean = 18, sd = 3), 1),
  rain = rbinom(n, 1, prob = 0.3),
  wave_action = sample(
    c("none", "low", "mod", "high"),
    size = n,
    replace = TRUE,
    prob = c(0.2, 0.3, 0.3, 0.2)
  ),
  water_clarity = sample(
    c("clear", "cloudy", "unknown"),
    size = n,
    replace = TRUE,
    prob = c(0.5, 0.3, 0.2)
  )
) %>%
  mutate(
    waterfowl_count = round(
      10 + 0.2 * wind_speed - 0.1 * air_temp + 
        0.3 * water_temp + 
        ifelse(wave_action == "low", 1, 
               ifelse(wave_action == "mod", 2, 
                      ifelse(wave_action == "high", 3, 0)
               )
        ) + 
        rain * 2 + 
        ifelse(water_clarity == "clear", 3, 
               ifelse(water_clarity == "cloudy", 1, 0)
        ) + rnorm(n, mean = 0, sd = 3)
    )
  )


#### Save data ####
write_csv(analysis_data, "data/00-simulated_data/simulated_data.csv")
