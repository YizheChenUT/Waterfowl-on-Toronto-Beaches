#### Preamble ####
# Purpose: Models factors influencing waterfowl counts on Toronto beaches
# Author: Yizhe Chen
# Date: 25 Nov 2024
# Contact: yz.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: No
# Any other information needed? No


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(arrow)


#### Read data ####
analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")

### Model data ####
# Building the first model: Using numeric predictors
first_model <-
  stan_glm(
    formula = water_fowl ~ wind_speed + air_temp + water_temp,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )

# Building the second model: Adding categorical and time predictors
second_model <-
  stan_glm(
    formula = water_fowl ~ wind_speed + air_temp + water_temp + wave_action +
      rain + water_clarity + beach_name + year + month,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )


#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)

saveRDS(
  second_model,
  file = "models/second_model.rds"
)


#### Model summary ####
# Print summary of the models
print(summary(first_model))
print(summary(second_model))
