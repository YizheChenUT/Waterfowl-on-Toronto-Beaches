#### Preamble ####
# Purpose: Downloads and saves the data from Open Data Toronto
# Author: Yizhe Chen
# Date: 24 Nov 2024
# Contact: yz.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: Install `opendatatoronto` and `tidyverse` R packages
# Any other information needed? No

#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)


#### Download data ####
# Get package details
package <- show_package("toronto-beaches-observations")

# List all resources for this package
resources <- list_package_resources("toronto-beaches-observations")

# Filter for datastore resources (CSV or GeoJSON formats)
datastore_resources <- resources |>
  filter(tolower(format) %in% c('csv', 'geojson'))

# Load the first datastore resource (CSV file for observations)
the_raw_data <- datastore_resources |>
  filter(row_number() == 1) |>
  get_resource()


#### Save data ####
# Save the downloaded raw data as a CSV file
write_csv(the_raw_data, "data/01-raw_data/raw_data.csv")
