# Script for exercise 4

#### Packages --------
library(dplyr)
library(tidyverse)
library(stats)
library(haven)
library(AER)


# set seed for reproducibility
set.seed(123)

#### Load Data --------
data <- read.csv2("Data/dataset.csv")

# 4a Instrumental Variables

iv_reg = ivreg(income_fe_t1_assignment ~ d_self_selection + age + gender | age + gender + d_assignment, data=data)
summary(iv_reg)
