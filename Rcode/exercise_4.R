# Script for exercise 4

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

# 4a Instrumental Variables
iv_reg_first <- lm(d_self_selection ~ children_german_primary + children_english_primary + age + gender + has_children, data = data)
summary(iv_reg_first)

iv_reg_reduced <- lm(income_fe_t1_self_selection ~ children_german_primary + children_english_primary + age + gender + has_children, data = data)
summary(iv_reg_reduced)

iv_reg_2sls <- ivreg(income_fe_t1_self_selection ~ + d_self_selection + children_english_primary + age + gender + has_children | age + gender + children_english_primary + has_children + children_german_primary, data=data)

summary(iv_reg_2sls)
stargazer(iv_reg_reduced, iv_reg_first, iv_reg_2sls, out = "Tables/instrumental.tex",
 column.labels   = c("OLS", "IV (First Stage)", "IV (Second Stage)"),
          column.separate = c(1, 1, 1),
          dep.var.labels  = c("Income in T = 1"," Self Selection", "Income in T = 1"),
          model.names = FALSE,
          covariate.labels = c("German Primary School", "Self Selection", "English Primary School", "Age", "Gender", "Has Children")
          )

 #add.lines = list(c("Fixed effects?", "No", "No"))