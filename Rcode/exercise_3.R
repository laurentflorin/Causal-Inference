# Script for exercise 3

#### Packages --------
library(dplyr)
library(tidyr)
library(stats)

# set seed for reproducability
set.seed(123)

# load data
data <- read.csv2("Data/dataset.csv")

# 3a is done in the data generation script

# 3b Balance of dataset

summary_assignment <- data %>% group_by(d_assignment) %>% select(d_assignment, age, gender, marital_status, social_benefits, education_level, years_in_ch, motivation, income_fe_t1_assignment) %>% summarise_each(list(mean=mean))
colnames(summary_assignment) <- colnames(data %>% select(d_assignment, age, gender, marital_status, social_benefits, education_level, years_in_ch, motivation, income_fe_t1_assignment))

summary_assignemt_tidy <- summary_assignment %>%  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(mean, se)



data %>% group_by(d_self_selection) %>% select(d_self_selection,age, gender, marital_status, social_benefits, education_level, years_in_ch, motivation, income_fe_t1_self_selection) %>% summarise_all(c("mean","sd"))

df.sum %>% gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, min, q25, median, q75, max, mean, sd) # reorder columns