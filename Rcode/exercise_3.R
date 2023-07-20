# Script for exercise 3


rm(list=ls()); gc()
#### Packages --------
library(dplyr)
library(tidyr)
library(stats)
library(qwraps2)
library(cobalt)
library(xtable)
library(stargazer)
library(data.table)
library(vtable)


devtools::source_gist("c4d1089a501d3567be9fb784b1c5a6ab") #function for nice descriptive table

# set seed for reproducability
set.seed(123)

# load data
data <- read.csv2("Data/dataset.csv")

data <- data %>% mutate(children_german_primary = as.factor(children_german_primary))

# 3a is done in the data generation script


# First we generate a summary table for assignment and self selection reporting the mean and the standard error
# of the covariates and the outcome

# summary statistics we want

myDescriptives = function(x) {
  x = as.numeric(x)
  n = round(length(x),0)
  m = round(mean(x, na.rm = TRUE),3)
  sd = round(sd(x),3)
  return(c(m, n, sd))
}

#prepare Datesets for createDescriptiveTable function
colnames = c("N", "Mean", "SE")
variables = list(c("age", "motivation", "distance", "education_level", "years_in_ch", "work_percentage", "children_german_primary", "income_fe_t1"),
                 c("age", "motivation", "distance", "education_level", "years_in_ch", "work_percentage", "children_german_primary", "income_fe_t1"))
labels = list(c("Age", "Motivation", "Distance", "Level of Education", "Years in Switzerland", "Work Percentage", "Children in German primary school", "Full time equivalent income"),
              c("Age", "Motivation", "Distance", "Level of Education", "Years in Switzerland", "Work Percentage", "Children in German primary school", "Full time equivalent income"))
data_assignment <- data %>% rename(d = d_assignment, income_fe_t1 = income_fe_t1_assignment) %>% select(age, motivation, distance, education_level, years_in_ch, work_percentage, children_german_primary, income_fe_t1,d)
data_self_selection <- data %>% rename(d = d_self_selection, income_fe_t1 = income_fe_t1_self_selection) %>% select(age, motivation, distance, education_level, years_in_ch, work_percentage, children_german_primary, income_fe_t1,d)
datasets <- list("Assignment" = as.data.table(data_assignment), "Self Selection" = as.data.table(data_self_selection))

# Generate Table
createDescriptiveTable(datasets,
                       summary_function = myDescriptives,
                       column_names = colnames,
                       variable_names = variables,
                       variable_labels = labels,
                       group_variable = "d",
                       arraystretch = 1.3,
                       tabcolsep = 3,
                       note = "Summary statics of covariates and outcome for assignment and self selection into treatment",
                       title = "Summary statistics",
                       label = "tab:summary",
                       file = "Tables/summary.tex")



# 3b Balance of dataset ------------------

# Generate a balancetable


sumtable(as_tibble(data_assignment), group = "d", group.test = T,vars = variables[[1]], labels = labels[[1]], out = "latex", title = "Balance Table for Random Assignment", file='Tables/balance_assignment.tex')
sumtable(data_self_selection, group = "d", group.test = T,vars = variables[[1]], labels = labels[[1]], out = "latex", title = "Balance Table for Self Selection", file='Tables/balance_self_select.tex')

#3c OLS Regression ---------------------

#OLS Regression with assignment
data_assignment <- data_assignment %>% mutate(age2 = age^2)
data_self_selection <- data_self_selection %>% mutate(age2 = age^2)

model_assignment <- lm(income_fe_t1 ~  age + age2  + education_level + years_in_ch + d,
                       data = as.data.frame(data_assignment))

writeLines(capture.output(stargazer(model_assignment, caption = "OLS regression table for assignment into treatment", label = "ols_assignment",table.placement = "H")), "Tables/ols_assignment.tex")
#print(xtable(summary(model_assignment), caption = "OLS regression table for assignment into treatment", label = "ols_assignment"),file="Tables/ols_assignment.tex", table.placement = getOption("xtable.table.placement", "H"))

#OLS Regression with self selection
model_self_select <- lm(income_fe_t1 ~ age + age2  + education_level + years_in_ch  + d,
                       data = as.data.frame(data_self_selection) )
summary(model_self_select)

writeLines(capture.output(stargazer(model_self_select, caption = "OLS regression table for self selection into treatment", label = "ols_self_select",table.placement = "H")), "Tables/ols_self_select.tex")
#print(xtable(summary(model_self_select),  caption = "OLS regression table for self selection into treatment", label = "ols_self_select"),file="Tables/ols_self_select.tex", table.placement = getOption("xtable.table.placement", "H"))

writeLines(capture.output(stargazer(model_assignment, model_self_select, label = "tab:.  ols",table.placement = "H", align=TRUE,
                                    covariate.labels = c("Age", "Age^2", "Education Level", "Years in Switzerland", "Treatment" ), dep.var.labels = "Full Time Equilvalent Income T+1", column.labels = c("Assignment", "Self Selection"))), "Tables/ols.tex")


