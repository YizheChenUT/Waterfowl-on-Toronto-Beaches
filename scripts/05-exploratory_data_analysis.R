#### Preamble ####
# Purpose: Perform exploratory data analysis (EDA) on cleaned Toronto Beaches Observations data
# Author: Yizhe Chen
# Date: 25 Nov 2024
# Contact: yz.chen@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned data is saved
# Any other information needed? No


#### Workspace setup ####
library(tidyverse)
library(ggcorrplot)


#### Read data ####
analysis_data <- read_csv("data/02-analysis_data/analysis_data.csv")

#### Exploratory Data Analysis ####

# Summary statistics for all variables
summary_stats <- analysis_data |> 
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                      sd = ~sd(.x, na.rm = TRUE),
                                      min = ~min(.x, na.rm = TRUE),
                                      max = ~max(.x, na.rm = TRUE))))

# Distribution of water_fowl
ggplot(analysis_data, aes(x = water_fowl)) +
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Waterfowl Counts",
       x = "Waterfowl Count",
       y = "Frequency") +
  theme_minimal()


# Correlation matrix for numeric variables
numeric_vars <- analysis_data |>
  select(where(is.numeric))

cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Heatmap for correlations
ggcorrplot(cor_matrix, lab = TRUE, lab_size = 3, method = "circle", 
           colors = c("blue", "white", "red"), title = "Correlation Matrix")

# Scatterplot: Waterfowl vs Wind Speed
ggplot(analysis_data, aes(x = wind_speed, y = water_fowl)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatterplot of Waterfowl vs Wind Speed",
       x = "Wind Speed (km/h)",
       y = "Waterfowl Count") +
  theme_minimal()


# Boxplot: Waterfowl by Wave Action
ggplot(analysis_data, aes(x = wave_action, y = water_fowl)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Boxplot of Waterfowl Count by Wave Action",
       x = "Wave Action",
       y = "Waterfowl Count") +
  theme_minimal()

