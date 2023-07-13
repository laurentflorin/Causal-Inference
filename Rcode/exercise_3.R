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

# 3b Balance of dataset ------------------

# First we generate a summary table for assignment and self selection reporting the mean and the standard error
# of the covariates and the outcome

# summary statistics we want
our_summary_assignment <-
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
       "Full time equivalent income at T" =
         list("mean"       = ~ round(mean(income_fe_t0 ),3),
              "se"       = ~ round(sd(income_fe_t0 )/sqrt(length(income_fe_t0)),3)),
       "Full time equivalent income at T+1" =
         list("mean"       = ~ round(mean(income_fe_t1_assignment),3),
              "se"       = ~ round(sd(income_fe_t1_assignment)/sqrt(length(income_fe_t1_assignment)),3))
  )

our_summary_assignment <-
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
       "Full time equivalent income at T" =
         list("mean"       = ~ round(mean(income_fe_t0 ),3),
              "se"       = ~ round(sd(income_fe_t0 )/sqrt(length(income_fe_t0)),3)),
       "Full time equivalent income at T+1" =
         list("mean"       = ~ round(mean(income_fe_t1_self_selection),3),
              "se"       = ~ round(sd(income_fe_t1_self_selection)/sqrt(length(income_fe_t1_self_selection)),3))
  )

summary_table_assignment <- summary_table(dplyr::group_by(data,d_assignment), our_summary1)
capture.output(print(summary_table_assignment),
               file = "Tables/summary_table_assignment.txt")

summary_table_self_select <- summary_table(dplyr::group_by(data,d_self_selection), our_summary1)
capture.output(print(summary_table_self_select),
               file = "Tables/summary_table_self_select.txt")

# Generate a balancetable

covs <- data %>% select(age, gender, marital_status, social_benefits, education_level, years_in_ch, zipcode, work_percentage, motivation) 

bal.tab(d_assignment ~ covs, data = data,
        binary = "std", continuous = "std",  stats = c("mean.diffs", "variance.ratios") ,
        thresholds = c(m = .1, v = 2), un = TRUE )

bal.tab(d_self_selection ~ covs, data = data,
        binary = "std", continuous = "std",  stats = c("mean.diffs", "variance.ratios") ,
        thresholds = c(m = .1, v = 2), un = TRUE )


# Balance Table with nearest Neighbor Matching

m.out <- MatchIt::matchit(d_assignment ~ age+ gender+ marital_status+ social_benefits+ education_level+ years_in_ch+ zipcode+ work_percentage+ motivation,
                          data = data)

bal.tab(m.out, thresholds = c(m = .1), un = TRUE)


m.out_self_select <- MatchIt::matchit(d_self_selection ~ age+ gender+ marital_status+ social_benefits+ education_level+ years_in_ch+ zipcode+ work_percentage+ motivation,
                          data = data)



bal.tab(m.out_self_select, thresholds = c(m = .1), un = TRUE)


love.plot(m.out, stats = c("mean.diffs", "variance.ratios"),
          thresholds = c(m = .1, v = 2), abs = TRUE, 
          binary = "std",
          var.order = "unadjusted")

love.plot(m.out_self_select, stats = c("mean.diffs", "variance.ratios"),
          thresholds = c(m = .1, v = 2), abs = TRUE, 
          binary = "std",
          var.order = "unadjusted")

#3c OLS Regression ---------------------

#OLS Regression with assignment
data <- data %>% mutate(age2 = age^2)

model_assignment <- lm(income_fe_t1_assignment ~ d_assignment + age + age2 + gender +
                         marital_status + social_benefits + education_level + years_in_ch + zipcode,
                       data = as.data.frame(data) )

summary(model_assignment)

model_self_select <- lm(income_fe_t1_self_selection ~ d_self_selection + age + age2 + gender +
                         marital_status + social_benefits + education_level + years_in_ch + zipcode,
                       data = as.data.frame(data) )

