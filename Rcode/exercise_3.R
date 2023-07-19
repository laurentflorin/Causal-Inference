# Script for exercise 3


rm(list=ls()); gc()
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
         list("mean"       = ~ round(mean(distance),3),
              "se"       = ~ round(sd(distance)/sqrt(length(distance)),3)),
       "Full time equivalent income at T" =
         list("mean"       = ~ round(mean(income_fe_t0 ),3),
              "se"       = ~ round(sd(income_fe_t0 )/sqrt(length(income_fe_t0)),3)),
       "Full time equivalent income at T+1" =
         list("mean"       = ~ round(mean(income_fe_t1_assignment),3),
              "se"       = ~ round(sd(income_fe_t1_assignment)/sqrt(length(income_fe_t1_assignment)),3))
  )

our_summary_self_select<-
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
         list("mean"       = ~ round(mean(distance),3),
              "se"       = ~ round(sd(distance)/sqrt(length(distance)),3)),
       "Full time equivalent income at T" =
         list("mean"       = ~ round(mean(income_fe_t0 ),3),
              "se"       = ~ round(sd(income_fe_t0 )/sqrt(length(income_fe_t0)),3)),
       "Full time equivalent income at T+1" =
         list("mean"       = ~ round(mean(income_fe_t1_self_selection),3),
              "se"       = ~ round(sd(income_fe_t1_self_selection)/sqrt(length(income_fe_t1_self_selection)),3))
  )

summary_table_assignment <- summary_table(dplyr::group_by(data,d_assignment), our_summary_assignment)
capture.output(print(summary_table_assignment),
               file = "Tables/summary_table_assignment.txt")

summary_table_self_select <- summary_table(dplyr::group_by(data,d_self_selection), our_summary_self_select)
capture.output(print(summary_table_self_select),
               file = "Tables/summary_table_self_select.txt")

# Generate a balancetable

covs <- data %>% select(age, gender, motivation, distance, education_level, years_in_ch, work_percentage, has_children, children_german_primary, children_english_primary,income_fe_t0) 

bal.tab(d_assignment ~ covs, data = data,
        binary = "std", continuous = "std",  stats = c("mean.diffs", "variance.ratios") ,
        thresholds = c(m = .1, v = 2), un = TRUE )

bal.tab(d_self_selection ~ covs, data = data,
        binary = "std", continuous = "std",  stats = c("mean.diffs", "variance.ratios") ,
        thresholds = c(m = .1, v = 2), un = TRUE )


# Balance Table with nearest Neighbor Matching

m.out <- MatchIt::matchit(d_assignment ~ age + motivation + distance + education_level + years_in_ch + work_percentage + has_children + children_german_primary + children_english_primary + income_fe_t0,
                          data = data, replace = TRUE)

summary(m.out)
bal.tab(m.out, thresholds = c(m = .1), un = TRUE)


m.out_self_select <- MatchIt::matchit(d_self_selection ~ age + gender + motivation + distance + education_level + years_in_ch + work_percentage + has_children + children_german_primary + children_english_primary + income_fe_t0,
                          data = data, replace = TRUE)

m.sum.self.select <- summary(m.out_self_select)
plot(m.sum.self.select, var.order = "unmatched")
plot(m.out_self_select, type = "density", which.xs = ~age + gender + motivation + distance + education_level + years_in_ch + work_percentage + has_children + children_german_primary + children_english_primary + income_fe_t0)


#3c OLS Regression ---------------------

#OLS Regression with assignment
data <- data %>% mutate(age2 = age^2)

model_assignment <- lm(income_fe_t1_assignment ~  age + age2  + education_level + years_in_ch + d_assignment,
                       data = as.data.frame(data) )

summary(model_assignment)

xtable::xtable(summary(model_assignment), caption = "OLS regression table for assignment into treatment")

#OLS Regression with self selection
model_self_select <- lm(income_fe_t1_self_selection ~ age + age2  + education_level + years_in_ch + d_self_selection,
                       data = as.data.frame(data) )
summary(model_self_select)

xtable::xtable(summary(model_self_select),  caption = "OLS regression table for self selection into treatment")



# Robust Standard Errors
cov.fit1 <- vcovHC(model_assignment, type = "HC")
rob.std.err1 <- sqrt(diag(cov.fit1))
cov.fit2 <- vcovHC(model_self_select, type = "HC")
rob.std.err2 <- sqrt(diag(cov.fit2))

# Rename d_assignment and d_self_selection to "d" such that they can be shown on same line in stargazer

names(model_assignment$coefficients)<- c("(Intercept)", "age", "age2", "eduation_level", "years_in_ch", "d")
names(model_self_select$coefficients)<- c("(Intercept)", "age", "age2", "eduation_level", "years_in_ch", "d")


stargazer(model_assignment, model_self_select,
 se = list(rob.std.err1, rob.std.err2),
 title = "OLS Regression Results",
 align = TRUE,
 dep.var.labels = c("Full Time Equilvalent Income T+1"),
 covariate.labels = c("Age","Age2","Education Level", "Years in Switzerland", "Treatment", "Constant"),
 column.labels = c("Assignment ", "Self Selection"),
 dep.var.caption = "",
 model.numbers = FALSE,
 out = "Tables/ols.tex",
 label = "tab:ols")
