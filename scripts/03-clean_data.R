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
library(lubridate)
library(arrow)


#### Define a function to remove outliers ####
remove_outliers <- function(x, na.rm = TRUE, threshold = 1.5) {
  if (na.rm) x <- na.omit(x)
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - threshold * iqr
  upper <- q3 + threshold * iqr
  x[x >= lower & x <= upper]
}

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
    data_collection_date = ymd(data_collection_date), # Ensure date column is in proper format
    year = year(data_collection_date), # Extract year
    month = month(data_collection_date, label = TRUE, abbr = TRUE), # Extract month as a factor
    wind_speed = as.numeric(wind_speed),
    air_temp = as.numeric(air_temp),
    water_temp = as.numeric(water_temp),
    water_fowl = as.numeric(water_fowl)
  ) |> # Convert numeric columns
  filter(
    !is.na(wind_speed), # Ensure no missing values in key numeric columns
    !is.na(air_temp),
    !is.na(water_temp),
    !is.na(water_fowl)
  ) |> # Drop rows with NA in key numeric columns
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
  group_by(beach_name) |> # Group by beach to handle outliers within each group
  filter(
    wind_speed %in% remove_outliers(wind_speed),
    air_temp %in% remove_outliers(air_temp),
    water_temp %in% remove_outliers(water_temp),
    water_fowl %in% remove_outliers(water_fowl)
  ) |> # Remove outliers
  ungroup() |> # Ungroup after filtering
  drop_na() # Drop any remaining rows with missing values


#### Save data ####
write_parquet(cleaned_data, "data/02-analysis_data/analysis_data.parquet")
