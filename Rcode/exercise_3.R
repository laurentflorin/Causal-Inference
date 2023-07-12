# Script for exercise 3

#### Packages --------
library(dplyr)
library(tidyr)
library(stats)
library(qwraps2)
library(cobalt)

# set seed for reproducability
set.seed(123)

# load data
data <- read.csv2("Data/dataset.csv")

# 3a is done in the data generation script

# 3b Balance of dataset

# First we generate a summary table for assignment and self selection reporting the mean and the standard error
# of the covariates and the outcome

# summary statistics we want
our_summary1 <-
  list("Age" =
         list("mean"       = ~ round(mean(age),3),
              "se"       = ~ round(sd(age)/sqrt(length(age)),3)),
       "Gender" =
         list("mean"       = ~ round(mean(gender),3),
              "se"       = ~ round(sd(gender)/sqrt(length(gender)),3)),
       "Marital Status" =
         list("mean"       = ~ round(mean(gender),3),
              "se"       = ~ round(sd(gender)/sqrt(length(gender)),3)),
       "Social Benefits" =
         list("mean"       = ~ round(mean(gender),3),
              "se"       = ~ round(sd(gender)/sqrt(length(gender)),3)),
       "Education Level" =
         list("mean"       = ~ round(mean(education_level),3),
              "se"       = ~ round(sd(education_level)/sqrt(length(education_level)),3)),
       "Years in Switzerland" =
         list("mean"       = ~ round(mean(years_in_ch),3),
              "se"       = ~ round(sd(years_in_ch)/sqrt(length(years_in_ch)),3)),
       "Years in Switzerland" =
         list("mean"       = ~ round(mean(years_in_ch),3),
              "se"       = ~ round(sd(years_in_ch)/sqrt(length(years_in_ch)),3)),
       "Work Percentage" =
         list("mean"       = ~ round(mean(work_percentage ),3),
              "se"       = ~ round(sd(work_percentage )/sqrt(length(work_percentage )),3)),
       "Work Percentage" =
         list("mean"       = ~ round(mean(work_percentage ),3),
              "se"       = ~ round(sd(work_percentage )/sqrt(length(work_percentage )),3)),
       "Living in Zurich" =
         list("mean"       = ~ round(mean(zipcode),3),
              "se"       = ~ round(sd(zipcode)/sqrt(length(zipcode)),3)),
       "Full time equivalent income at T+1" =
         list("mean"       = ~ round(mean(income_fe_t1_assignment),3),
              "se"       = ~ round(sd(income_fe_t1_assignment)/sqrt(length(income_fe_t1_assignment)),3))
       
  )

summary_table_assignment <- summary_table(dplyr::group_by(data,d_assignment), our_summary1)
capture.output(print(summary_table_assignment),
               file = "Tables/summary_table_assignment.txt")

summary_table_self_select <- summary_table(dplyr::group_by(data,d_self_selection), our_summary1)
capture.output(print(summary_table_self_select),
               file = "Tables/summary_table_self_select.txt")

# Generate a balancetable

