#### Preamble ####
# Purpose: Tests the structure and validity of the analysis dataset for Toronto beach observations.
# Author: Yizhe Chen
# Date: 1 Dec 2024
# Contact: yz.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse`, `testthat`, and `validate` packages must be installed.
# Any other information needed? No


#### Workspace setup ####
library(tidyverse)
library(testthat)
library(validate)
library(arrow)


analysis_data <- read_parquet("data/02-analysis_data/analysis_data.parquet")


#### Test data ####

# Test the dataset dimensions
test_that("Dataset has 12 columns", {
  expect_equal(ncol(analysis_data), 12)
})

# Validate the date range
test_that("'data_collection_date' is within expected range", {
  expect_true(all(analysis_data$data_collection_date >= as.Date("2005-01-01") & 
                    analysis_data$data_collection_date <= as.Date("2025-01-01")))
})

# Validate numerical columns (wind_speed, air_temp, water_temp, water_fowl)
test_that("Numerical columns are within reasonable ranges", {
  expect_true(all(analysis_data$wind_speed >= 0 & analysis_data$wind_speed <= 30))
  expect_true(all(analysis_data$air_temp >= -10 & analysis_data$air_temp <= 40))
  expect_true(all(analysis_data$water_temp >= 0 & analysis_data$water_temp <= 30))
  expect_true(all(analysis_data$water_fowl >= 0))
})

# Validate categorical columns
valid_wave_actions <- c("none", "low", "mod", "high")
valid_water_clarities <- c("clear", "cloudy", "unknown")
test_that("'wave_action' and 'water_clarity' contain valid categories", {
  expect_true(all(analysis_data$wave_action %in% valid_wave_actions))
  expect_true(all(analysis_data$water_clarity %in% valid_water_clarities))
})

# Check for realistic rain distribution
test_that("'rain' column contains only binary values", {
  expect_true(all(analysis_data$rain %in% c(0, 1)))
  expect_true(mean(analysis_data$rain) >= 0.2 & mean(analysis_data$rain) <= 0.4)
})

# Check uniqueness of rows
test_that("All rows are unique", {
  expect_equal(nrow(analysis_data), nrow(distinct(analysis_data)))
})

# Statistical distribution checks
test_that("Numerical columns have reasonable mean and standard deviation", {
  expect_true(abs(mean(analysis_data$wind_speed) - 10) < 2)
  expect_true(abs(sd(analysis_data$wind_speed) - 3) < 3)
  expect_true(abs(mean(analysis_data$air_temp) - 20) < 2)
  expect_true(abs(sd(analysis_data$air_temp) - 5) < 1)
  expect_true(abs(mean(analysis_data$water_temp) - 18) < 3)
})

# Validate relationships between columns
test_that("Relationships between variables are consistent", {
  # Higher water temperature should correlate positively with waterfowl count
  expect_true(cor(analysis_data$water_temp, analysis_data$water_fowl) > 0)
  # Wind speed should not negatively affect waterfowl count too strongly
  expect_true(cor(analysis_data$wind_speed, analysis_data$water_fowl) > -0.2)
})

# Advanced data validation using `validate` package
rules <- validator(
  ncol(analysis_data) == 12,
  all(!is.na(analysis_data)),
  all(analysis_data$wind_speed >= 0 & analysis_data$wind_speed <= 30),
  all(analysis_data$rain %in% c(0, 1)),
  all(analysis_data$wave_action %in% valid_wave_actions),
  all(analysis_data$water_clarity %in% valid_water_clarities)
)

test_that("Validation rules are satisfied", {
  results <- confront(analysis_data, rules)
  expect_true(all(summary(results)$fails == 0))
})