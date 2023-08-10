# Script for exercise 5

#### Packages --------
library(dplyr)
library(tidyverse)
library(stats)
library(haven)
library(AER)
library(stargazer)
library(scales)
library(fastDummies)
library(lfe)
library(did)
library(ggiplot)


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
  mutate(age = age + (year - 2017),
  years_in_ch = years_in_ch + (year - 2017),
  post = ifelse(year >= 2020, 1, 0),
  # individuals who were not married in the previous year have a chance of being married in the following year of 10 percent
  marital_status = ifelse(lag(marital_status, default = 0) == 0 & runif(n()) <= 0.1, 1, marital_status)) %>%
  ungroup()


# Equation (4): Treatment Effect
u_0 <- rnorm(n, 0, 50) #error term U_i(0)
u_1 <- rnorm(n, 0, 50) #error term U_i(1)
delta <- 400 + u_1 - u_0

# Calculate Income for each period
income <- 4000  + delta * (panel_data$post * panel_data$zurich) + 50 * panel_data$zurich + 20 * panel_data$post + 4 * panel_data$age + 0.05 * panel_data$age^2 + + 30 * panel_data$education_level + 3 * panel_data$years_in_ch + 120 * panel_data$motivation + 0.5 * panel_data$year + u_0


panel_data <- panel_data %>%
mutate(income = income,
        age2 = age^2)


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


# 5b Did OLS
model_did <- lm(income ~ zurich + post + (zurich * post) + age + age2 + gender + education_level + years_in_ch + year, data = panel_data)
summary(model_did)

# Robust Standard Errors
cov.fit.did <- vcovHC(model_did, type = "HC")
rob.std.did <- sqrt(diag(cov.fit.did))



stargazer(model_did,
 se = list(rob.std.did),
 title = "OLS Regression Results",
 align = TRUE,
 dep.var.labels = c("Full Time Equilvalent Income T+1"),
 covariate.labels = c("Zurich", "Post", "Age","Age2", "Gender", "Education Level", "Years in Switzerland", "Year", "Zurich x Post", "Constant"),
 #column.labels = c("Assignment ", "Self Selection"),
 dep.var.caption = "",
 model.numbers = FALSE,
 table.placement = "H",
 out = "Tables/did.tex",
 label = "tab:did")


# 5c Event study

panel_data <- panel_data %>%
  mutate(rel_year = years_in_ch - 3) %>% 
    mutate(rel_year = ifelse(rel_year == -Inf, NA, rel_year))%>% 
    dummy_cols(select_columns = "rel_year") %>% 
    mutate(across(starts_with("rel_year_"), ~replace_na(., 0))) %>% 
    # generate pre and post dummies
    mutate(Pre = ifelse((rel_year < 0) * (!is.na(rel_year)), 1, 0),
           Post = ifelse((rel_year >= 0) * (!is.na(rel_year)), 1, 0)) %>%
    mutate(Pre = ifelse(is.na(Pre), 0, Pre),
           Post = ifelse(is.na(Post), 0, Post),
           treat_year = zurich * rel_year)


# Calculate Income for each period
income_event <- 4000  - 100 * (panel_data$Pre * panel_data$zurich) + delta * (panel_data$Post * panel_data$zurich) + 50 * panel_data$zurich + 20 * panel_data$rel_year + 4 * panel_data$age + 0.05 * panel_data$age^2 + + 30 * panel_data$education_level + 120 * panel_data$motivation + 3 * panel_data$years_in_ch + 0.5 * panel_data$year + u_0

panel_data <- panel_data %>%
mutate(income = income_event)

model_event <- feols(income ~ i(treat_year, zurich, ref = 0) + age + age2 + gender + education_level + years_in_ch + year, data = panel_data)
summary(model_event)


# save love plot
png(filename="Graphs/event_plot.png")
iplot(model_event)
dev.off()


modelsummary::modelsummary(model_event, 
  vcov = "HC",
  title = "OLS Regression Results",
  coef_rename = c("Constant",
                  "Year t-3 x Zurich",
                  "Year t-2 x Zurich",
                  "Year t-1 x Zurich",
                  "Year t+1 x Zurich",
                  "Year t+2 x Zurich",
                  "Year t+3 x Zurich",
                  "Year t+4 x Zurich",
                  "Year t+5 x Zurich",
                  "Year t+6 x Zurich",
                  "Year t+7 x Zurich",
                  "Year t+8 x Zurich",
                  "Year t+9 x Zurich",
                  "Year t+10 x Zurich",
                  "Year t+11 x Zurich",
                  "Year t+12 x Zurich",
                  "Age", "Age2", "Gender", "Education Level", "Years in Switzerland", "Year"),
  output = "Tables/event.tex")

stargazer(model_event,
 title = "OLS Regression Results",
 align = TRUE,
 dep.var.labels = c("Full Time Equilvalent Income T+1"),
 #covariate.labels = c("Zurich", "Post", "Age","Age2", "Gender", "Education Level", "Years in Switzerland", "Year", "Zurich x Post", "Constant"),
 dep.var.caption = "",
 model.numbers = FALSE,
 table.placement = "H",
 out = "Tables/event.tex",
 label = "tab:event")


# 5d Not Parallel Trend
# Calculate Income for each period
income_nonparallel <- 4000  + 30 * ((panel_data$year - 2017) * panel_data$zurich) + delta * (panel_data$post * panel_data$zurich) + 50 * panel_data$zurich + 20 * panel_data$post + 4 * panel_data$age + 0.05 * panel_data$age^2 + + 30 * panel_data$education_level + 3 * panel_data$years_in_ch + 120 * panel_data$motivation + 2 * (panel_data$year - 2017) + u_0

panel_data <- panel_data %>%
mutate(income = income_nonparallel)


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
ggsave("Graphs/dd_nonparallel_plot.png", plot = last_plot(), width = 8, height = 6)


# Did OLS
model_nonparallel <- lm(income ~ zurich + post + (zurich * post) + age + age2 + gender + education_level + years_in_ch + year, data = panel_data)
summary(model_nonparallel)

# Robust Standard Errors
cov.fit.nonparallel <- vcovHC(model_nonparallel, type = "HC")
rob.std.nonparallel <- sqrt(diag(cov.fit.nonparallel))


stargazer(model_nonparallel,
 se = list(rob.std.nonparallel),
 title = "OLS Regression Results",
 align = TRUE,
 dep.var.labels = c("Full Time Equilvalent Income T+1"),
 covariate.labels = c("Zurich", "Post", "Age","Age2", "Gender", "Education Level", "Years in Switzerland", "Year", "Zurich x Post", "Constant"),
 #column.labels = c("Assignment ", "Self Selection"),
 dep.var.caption = "",
 model.numbers = FALSE,
 table.placement = "H",
 out = "Tables/nonparallel.tex",
 label = "tab:nonparallel")
