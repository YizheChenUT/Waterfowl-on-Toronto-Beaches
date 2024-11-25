#### Preamble ####
# Purpose: Cleans the raw Toronto Beaches Observations data for analysis
# Author: Yizhe Chen
# Date: 24 Nov 2024
# Contact: yz.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: Raw data are downloaded
# Any other information needed? No

#### Workspace setup ####
library(tidyverse)
library(janitor)

#### Clean data ####
# Read raw data
raw_data <- read_csv("data/01-raw_data/raw_data.csv")

# Clean and preprocess data
cleaned_data <-
  raw_data |>
  clean_names() |> # Standardize column names
  select(
    data_collection_date,
    beach_name,
    wind_speed,
    wind_direction,
    air_temp,
    rain,
    water_temp,
    water_fowl,
    wave_action,
    water_clarity
  ) |> # Select relevant columns
  mutate(
    wind_speed = as.numeric(wind_speed),
    air_temp = as.numeric(air_temp),
    water_temp = as.numeric(water_temp),
    water_fowl = as.numeric(water_fowl)
  ) |> # Convert numeric columns
  mutate(
    rain = if_else(rain == "Yes", 1, if_else(rain == "No", 0, NA_real_)),
    wave_action = str_to_lower(wave_action) |> 
      factor(levels = c("none", "low", "mod", "high")),
    water_clarity = case_when(
      str_detect(water_clarity, regex("clear|clean", ignore_case = TRUE)) ~ "clear",
      str_detect(water_clarity, regex("cloudy|muddy|turbid", ignore_case = TRUE)) ~ "cloudy",
      TRUE ~ "unknown"
    )
  ) |> # Process categorical variables
  filter(!is.na(water_fowl)) |> # Remove rows where waterFowl is missing
  drop_na() # Drop remaining rows with missing values


#### Save data ####
write_csv(cleaned_data, "data/02-analysis_data/analysis_data.csv")
