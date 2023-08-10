# R Code Exercises 3 to 5


rm(list=ls()); gc()

#### Packages --------
library(dplyr)
library(tidyr)
library(tidyverse)
library(stats)
library(qwraps2)
library(cobalt)
library(xtable)
library(stargazer)
library(data.table)
library(vtable)
library(sandwich)
library(haven)
library(AER)
library(MatchIt)
library(marginaleffects)
library(broom)
library(rdrobust)
library(rddensity)
library(modelsummary)
library(estimatr)
library(fastDummies)
library(lfe)
library(did)
library(ggiplot)

devtools::source_gist("c4d1089a501d3567be9fb784b1c5a6ab") #function for nice descriptive table


# set seed for reproducibility
set.seed(123)

#### Load Data --------
data <- read.csv2("Data/dataset.csv")

#### Exercise 3 --------

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



# 3b Balance of dataset 

# Generate a balancetable


sumtable(as_tibble(data_assignment), group = "d", group.test = T,vars = variables[[1]], labels = labels[[1]], out = "latex", title = "Balance Table for Random Assignment", file='Tables/balance_assignment.tex')
sumtable(data_self_selection, group = "d", group.test = T,vars = variables[[1]], labels = labels[[1]], out = "latex", title = "Balance Table for Self Selection", file='Tables/balance_self_select.tex')

#3c OLS Regression 

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

#writeLines(capture.output(stargazer(model_self_select, caption = "OLS regression table for self selection into treatment", label = "ols_self_select",table.placement = "H")), "Tables/ols_self_select.tex")
#print(xtable(summary(model_self_select),  caption = "OLS regression table for self selection into treatment", label = "ols_self_select"),file="Tables/ols_self_select.tex", table.placement = getOption("xtable.table.placement", "H"))

#writeLines(capture.output(stargazer(model_assignment, model_self_select, label = "tab:.  ols",table.placement = "H", align=TRUE,
#                                    covariate.labels = c("Age", "Age^2", "Education Level", "Years in Switzerland", "Treatment" ), dep.var.labels = "Full Time Equilvalent Income T+1", column.labels = c("Assignment", "Self Selection"))), "Tables/ols.tex")


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
          table.placement = "H",
          out = "Tables/ols.tex",
          label = "tab:ols")


#### Exercise 4 ------

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

#4b RDD

full_data <- read.csv2("Data/dataset.csv")

# Compliance histogram

distance_bins <- full_data %>%
  mutate(distance_binned = cut(distance, breaks = seq(0, 1, 0.05))) %>%
  # Group by each of the new bins and treatment  status
  group_by(distance_binned, d_self_selection) %>%
  # Count how many people are in each test bin + didn't self-select
  summarize(n = n()) %>%
  # Make this summarized data wider so that there's a column for treatment  and no treatment 
  pivot_wider(names_from = "d_self_selection", values_from = "n", values_fill = 0) %>%
  rename(treatment_yes = `1`, treatment_no = `0`) %>%
  # Find the probability of treatment in each bin by taking
  # the count of yes / count of yes + count of no
  mutate(propoprtion_treatment = treatment_yes / (treatment_yes + treatment_no))


ggplot(distance_bins, aes(x = distance_binned, y = propoprtion_treatment)) +
  geom_col() +
  geom_vline(xintercept = 0.3) +
  labs(x = "Distance to Language Center", y = "Share of People Taking the German Course")+
  theme(text = element_text(size = 16)) +
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave("Graphs/hist_compliers.png", plot = last_plot(), width = 8, height = 6)

# Discontinuity in outcome across running variable

# Outcome: Income
# Running variable: distance to ZCH
# Cutoff value: 0.3 
cutoff_value <- 0.4

### FUZZY plot 

full_data_rdd <- full_data[1:15000,] %>% #reducing the sample for visualization purposes
  mutate(d_self_selection = factor(d_self_selection)) #factoring for the plot

ggplot(full_data_rdd , aes(x= distance, y=income_fe_t1_self_selection, color = d_self_selection))+
  geom_vline(xintercept = 0.4, lwd = 0.75 )+
  geom_point(size = 0.75, alpha = 0.35) +
  scale_color_manual(values = c("orange", "blue")) +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15))))+
  geom_smooth(method = "lm", data = filter(full_data_rdd, d_self_selection == 1)) +
  geom_smooth(method = "lm", data = filter(full_data_rdd, d_self_selection == 0))+
  labs(x = "Distance to ZCH", y = "Income", color = "Selection to Treatment")+
  theme(text = element_text(size = 16)) 
ggsave("Graphs/fuzzy_plot.png", plot = last_plot(), width = 8, height = 6)

### FUZZY plot around the cutoff value

#Plot with regression of values around the cutoff
ggplot(full_data_rdd , aes(x= distance, y=income_fe_t1_self_selection, color = d_self_selection))+
  geom_vline(xintercept = 0.4, lwd = 2)+
  geom_point(size = 0.75, alpha = 0.15) +
  scale_color_manual(values = c("orange", "blue")) +
  guides(color = guide_legend(override.aes = list(shape = c(15, 15))))+
  geom_smooth(method = "lm", data = filter(full_data_rdd,
                                           distance >= 0.35,
                                           distance < cutoff_value,
                                           d_self_selection == 1)) +
  geom_smooth(method = "lm", data = filter(full_data_rdd, 
                                           distance > cutoff_value,
                                           distance <= 0.45,
                                           d_self_selection ==0))+
  labs(x = "Distance to ZCH", y = "Income", color = "Selection to Treatment")


# Size of the effect - Measuring the GAP

# Measure the GAP around the cutoff
# Parametric method: regression
# center our running variable around the cutoff 

distance_centered <- full_data %>% 
  mutate(distance_center = distance - cutoff_value,
         below_cutoff = distance <= cutoff_value)

# FUZZY assumption

model_fuzzy <- iv_robust(income_fe_t1_self_selection ~  distance_center + d_self_selection | distance_center + below_cutoff,
                         data = filter(distance_centered,
                                       distance_center <= 0.05 &
                                         distance_center >= -0.05))
tidy(model_fuzzy)
summary(model_fuzzy)
#Intercept: 4350 --> income at distance_center = 0 --> distance = cutoff_value & d_self_selection = 0
#d_self_selection: 414 --> treatment jumps 414 from one group to another --> causal effect 

#Generating the table for results


names(model_self_select$coefficients)<- c("(Intercept)", "age", "age2", "eduation_level", "years_in_ch", "d")


stargazer(model_fuzzy,
          title = "RDD fuzzy estimates",
          align = TRUE,
          dep.var.caption = "Dependent Variable:",
          model.numbers = FALSE,
          table.placement = "H",
          out = "Tables/rdd.tex",
          label = "tab:ols")



stargazer(results,
          title = "RDD fuzzy estimates",
          column.labels = c("Estimate", "Std. Error", "Statistic", "P-value", "95% CI Lower", "95% CI Upper", "DF"),
          align = TRUE,
          dep.var.caption = "Dependent Variable:",
          dep.var.labels = c("Income in T = 1","Self Selection", "Income in T = 1"),
          rownames = FALSE,
          out = "Tables/rdd.tex"
)

# 4c 

data <- data %>% mutate(age2 = age^2)

data <- data %>% rename(distance_to_center = distance)

# Using Matching approach to get a balanced data set
# We use nearest neighbor matching based on the propensity score
# We match on age, distance, education level, years in ch, work percentage and children in german primary
#we can not match on motivation

#matching
m.out_self_select <- MatchIt::matchit(d_self_selection ~ age + distance_to_center + education_level + years_in_ch + work_percentage + children_german_primary ,
                                      data = data, replace = FALSE, method = "nearest", estimand = "ATT")
new.names <- c(age = "Age",
               distance_to_center = "Distance",
               education_level = "Level of Education",
               years_in_ch = "Years in Switzerland",
               work_percentage = "Work Percentage",
               children_german_primary = "Children in German primary school")

m.sum.self.select <- summary(m.out_self_select)

# save love plot
png(filename="Graphs/love_plot.png")
love.plot(m.out_self_select, binary = "std", var.names = new.names, var.order = "unadjusted", abs = T)
dev.off()


m.data <- match.data(m.out_self_select) # get matched data set

t.test(income_fe_t1_self_selection ~ d_self_selection, data = m.data)


# fit same model as with ols but with matched data
fit <- lm(income_fe_t1_self_selection ~  (age + age2  + education_level + years_in_ch)*d_self_selection , 
          data = as.data.frame(m.data), weights = weights)

att <- avg_comparisons(fit,
                       vcov = ~subclass,
                       variables = "d_self_selection",
                       newdata = subset(m.data, d_self_selection == 1),
                       wts = "weights")

avg_predictions(fit,
                vcov = ~subclass,
                variables = "d_self_selection",
                newdata = subset(m.data, d_self_selection == 1),
                wts = "weights")


print(xtable(att, caption = "ATT estimate with matching approach", label = "tab:att_matching"),file="Tables/att_matching.tex", table.placement = getOption("xtable.table.placement", "H"))

#balance table for matched data
sumtable(m.data, group = "d_self_selection", group.test = T,vars = c("age", "motivation", "distance", "education_level", "years_in_ch", "work_percentage", "children_german_primary", "income_fe_t1_self_selection"), 
         labels = c("Age", "Motivation", "Distance", "Level of Education", "Years in Switzerland", "Work Percentage", "Children in German primary school", "Full time equivalent income"), out = "latex", title = "Balance Table of Matched Data", file='Tables/balance_matched.tex')


#### Exercie 5 -----
n <- 150000

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
