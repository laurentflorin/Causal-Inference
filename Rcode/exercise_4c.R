# Script for exercise 4c


rm(list=ls()); gc()
#### Packages --------
library(dplyr)
library(tidyr)
library(stats)
library(qwraps2)
library(cobalt)
library(xtable)
library(MatchIt)
library(marginaleffects)


# set seed for reproducability
set.seed(123)

# load data
data <- read.csv2("Data/dataset.csv")

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


print(xtable(att, caption = "ATT estimate with matching approach", label = "tab:att_mathcing"),file="Tables/att_matching.tex", table.placement = getOption("xtable.table.placement", "H"))

#balance table for matched data
sumtable(m.data, group = "d_self_selection", group.test = T,vars = c("age", "motivation", "distance", "education_level", "years_in_ch", "work_percentage", "children_german_primary", "income_fe_t1_self_selection"), 
         labels = c("Age", "Motivation", "Distance", "Level of Education", "Years in Switzerland", "Work Percentage", "Children in German primary school", "Full time equivalent income"), out = "latex", title = "Balance Table of Matched Data", file='Tables/balance_matched.tex')



