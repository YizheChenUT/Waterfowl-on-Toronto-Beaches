#### Preamble ####
# Purpose: Tests the structure and validity of the simulated Toronto beaches dataset.
# Author: Yizhe Chen
# Date: 1 Dec 2024
# Contact: yz.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Install and load `testthat` and `pointblank` packages.
# - Simulated data must be saved as `simulated_data.csv`.
# Any other information needed? No


#### Workspace setup ####
library(testthat)
library(pointblank)
library(tidyverse)

# Load simulated data
simulated_data <- read_csv("data/00-simulated_data/simulated_data.csv")

#### Test Suite using testthat ####

test_that("Dataset structure and basic properties", {
  # Check the dataset has the expected number of columns
  expect_equal(ncol(simulated_data), 9, label = "The dataset should have 9 columns.")
  
  # Verify column names
  expected_columns <- c(
    "data_collection_date", "beach_name", "wind_speed",
    "air_temp", "water_temp", "rain", "wave_action", 
    "water_clarity", "waterfowl_count"
  )
  expect_equal(names(simulated_data), expected_columns, label = "Column names are incorrect.")
  
  # Ensure the dataset has at least 1000 rows
  expect_true(nrow(simulated_data) >= 1000, label = "The dataset should have at least 1000 rows.")
})

test_that("Data values are valid", {
  # Check 'data_collection_date' for valid date format
  expect_true(all(!is.na(as.Date(simulated_data$data_collection_date))),
              label = "'data_collection_date' should contain valid dates.")
  
  # Check numeric columns for valid ranges
  expect_true(all(simulated_data$wind_speed >= 0), label = "'wind_speed' should be non-negative.")
  expect_true(all(simulated_data$air_temp > -50 & simulated_data$air_temp < 60),
              label = "'air_temp' should be within a realistic range.")
  expect_true(all(simulated_data$water_temp > 0 & simulated_data$water_temp < 40),
              label = "'water_temp' should be within a realistic range.")
  
  # Ensure binary column 'rain' contains only 0 or 1
  expect_true(all(simulated_data$rain %in% c(0, 1)), label = "'rain' should be binary (0 or 1).")
  
  # Validate categorical columns
  valid_wave_actions <- c("none", "low", "mod", "high")
  expect_true(all(simulated_data$wave_action %in% valid_wave_actions),
              label = "'wave_action' should contain valid categories.")
  
  valid_water_clarity <- c("clear", "cloudy", "unknown")
  expect_true(all(simulated_data$water_clarity %in% valid_water_clarity),
              label = "'water_clarity' should contain valid categories.")
  
  # Ensure waterfowl counts are non-negative
  expect_true(all(simulated_data$water_fowl >= 0), 
              label = "'waterfowl_count' should be non-negative.")
})

test_that("Missing and duplicate values", {
  # Check for missing values
  expect_true(all(!is.na(simulated_data)), label = "The dataset should not contain missing values.")
  
  # Check for duplicate rows
  expect_true(nrow(simulated_data) == n_distinct(simulated_data), 
              label = "The dataset should not contain duplicate rows.")
})


#### Test Suite using pointblank ####
# Validate dataset with pointblank
agent <- create_agent(tbl = simulated_data) %>%
  col_schema_match(
    schema = col_schema(
      data_collection_date = "character",
      beach_name = "character",
      wind_speed = "numeric",
      air_temp = "numeric",
      water_temp = "numeric",
      rain = "integer",
      wave_action = "character",
      waterfowl_count = "numeric"
    )
  ) |>
  col_vals_in_set(
    columns = vars(beach_name),
    set = c(
      "Sunnyside Beach", "Woodbine Beach", "Cherry Beach", 
      "Hanlan's Point Beach", "Marie Curtis Park Beach", 
      "Kew Balmy Beach", "Centre Island Beach"
    )
  ) |>
  col_vals_between(
    columns = vars(wind_speed),
    left = 0, right = 20
  ) |>
  col_vals_between(
    columns = vars(air_temp),
    left = -50, right = 60
  ) |>
  col_vals_between(
    columns = vars(water_temp),
    left = 0, right = 40
  ) |>
  col_vals_in_set(
    columns = vars(wave_action),
    set = c("none", "low", "mod", "high")
  ) |>
  col_vals_in_set(
    columns = vars(water_clarity),
    set = c("clear", "cloudy", "unknown")
  ) |>
  col_vals_gte(
    columns = vars(waterfowl_count),
    value = 0
  ) |>
  col_vals_lte(
    columns = vars(waterfowl_count),
    value = 100
  ) |>
  interrogate()

# Print pointblank summary
print(agent)