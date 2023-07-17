# Script for exercise 5

#### Packages --------
library(dplyr)
library(tidyverse)
library(stats)
library(haven)
library(AER)
library(stargazer)


# set seed for reproducibility
set.seed(123)

#### Load Data --------
data <- read.csv2("Data/dataset.csv")

# 5 Panel Date

# Produce Panel Data -------
# Create a sequence of years from 2017 to 2022
years <- seq(2017, 2022)

data$year <- 2019

# Duplicate each ID for each year
panel_data <- lapply(years, function(yr) {
  temp_data <- data
  temp_data$year <- yr
  return(temp_data)
})

# Combine the panel data into a single dataframe
panel_data <- do.call(rbind, panel_data)

# Sort the panel data by ID and year
panel_data <- panel_data[order(panel_data$id, panel_data$year), ]

# Reset row names
rownames(panel_data) <- NULL

# View the resulting panel data
head(panel_data, 18)

# Adjust the age and marital variable for each year
panel_data <- panel_data %>%
  group_by(id) %>%
  mutate(age = age + (year - 2018),
  # individuals who were not married in the previous year have a chance of being married in the following year of 10 percent
    marital_status = ifelse(lag(marital_status, default = 0) == 0 & runif(n()) <= 0.1, 1, marital_status)) %>%
  ungroup()

# Redo self-selection for each period
d_star_values <- calculate_d_star(panel_data, n)
d_self_selection_values <- calculate_d_self_selection(d_star_values, panel_data, n)
income_values <- calculate_income_fe_t1_self_selection(panel_data, d_self_selection_values, delta)

panel_data <- panel_data %>%
mutate(d_star = d_star_values,
d_self_selection = d_self_selection_values,
income = income_values)

# Generate random variation for income
panel_data <- panel_data %>%
  group_by(id) %>%
  mutate(income_variation = rnorm(n(), mean = 0, sd = 100)) %>%
  ungroup()

# Increase income by 10% for Zurich (distance < 0.2) after 2020
panel_data <- panel_data %>%
  mutate(income = ifelse(year >= 2020 & distance < 0.2, income * 1.1, income))  

write.csv2(panel_data, file = "Data/paneldata.csv")
