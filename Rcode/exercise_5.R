# Script for exercise 5

#### Packages --------
library(dplyr)
library(tidyverse)
library(stats)
library(haven)
library(AER)
library(stargazer)
library(scales)


# set seed for reproducibility
set.seed(123)

#### Load Data --------
data <- read.csv2("Data/dataset.csv") %>%
mutate(marital_status = rbinom(150000, size = 1, prob = 0.3), # probability of being married 30%
        zurich = rbinom(150000, size = 1, prob = 0.2)
)

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
n <- nrow(panel_data)
# Equation 2
d_star <- -50 -  panel_data$age - panel_data$education_level - 5 * panel_data$work_percentage + 90 * panel_data$motivation - 20 * panel_data$distance + 40 * panel_data$children_german_primary + rnorm(n, 0, 20)

# Equation (3)
d_self_selection <- d_star
for (i in 1:n){
  if ((d_star[i] > 0 && panel_data$zurich[i] == 1 && panel_data$motivation[i] > 0.85)) {
    d_self_selection[i] <- 1
  } else {
    d_self_selection[i] <- 0
    }
}

# Equation (4): Treatment Effect
u_0 <- rnorm(n, 0, 50) #error term U_i(0)
u_1 <- rnorm(n, 0, 50) #error term U_i(1)
delta <- 400 + u_1 - u_0

# Calculate Income based on self-selection for each period 
income <- 4000  + 4 * panel_data$age + 0.05 * panel_data$age^2  + 30 * panel_data$education_level + 3 * panel_data$years_in_ch + 120 * panel_data$motivation + d_self_selection * delta + 10 * (panel_data$year - 2017) + u_0


panel_data <- panel_data %>%
mutate(d_star = d_star,
d_self_selection = d_self_selection,
income = income)


# Increase income by 10% for Zurich (distance < 0.2) after 2020 (T=4)
panel_data <- panel_data %>%
  mutate(income = ifelse(year >= 2020 & zurich == 1, income * 1.1, income))

## Dif-In-Dif --------
did_data <- panel_data %>%
  group_by(year, zurich) %>%
  summarize(avg_income = mean(income, na.rm = TRUE)) %>%
  ungroup()

did_data <- did_data %>%
  mutate(DiD_effect = avg_income - lag(avg_income))

# Convert d_self_selection to a factor variable
did_data$zurich <- factor(did_data$zurich)

ggplot(did_data, aes(x = year, y = avg_income, color = zurich, group = zurich)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = c(2019.5), linetype = "dashed", color = "gray") +
  labs(x = "Year", y = "Average Income", title = "",
       color = "Lives in Zurich") +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(labels = comma_format()) +  # Add thousand separator to y-axis labels
  scale_color_manual(values = c("orange", "blue"))   # Specify custom colors
ggsave("Graphs/dd_plot.png", plot = last_plot(), width = 8, height = 6)
