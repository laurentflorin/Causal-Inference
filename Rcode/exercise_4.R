# Script for exercise 4

#### Packages --------
library(dplyr)
library(tidyverse)
library(stats)
library(haven)
library(AER)
library(stargazer)

#-------------------------------------------------------------------------------
#### A) Instrumental-Variable: 
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
#### B) RDD
#-------------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(rdrobust)
library(rddensity)
library(modelsummary)
library(estimatr)
library(stargazer)

full_data <- read.csv2("Data/dataset.csv")

# Compliance histogram
#-------------------------------------------------------------------------------
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
  labs(x = "Distance to Zürich", y = "Proportion of people participating in the German course")+
  theme(text = element_text(size = 16)) 

# Discontinuity in outcome across running variable
#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
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

results <- data.frame(
  term = c("(Intercept)", "distance_center", "d_self_selection"),
  estimate = c(4346.50098, 64.69395, 463.99130),
  std.error = c(4.776784, 59.367046, 51.566278),
  statistic = c(909.921984, 1.089728, 8.997960),
  p.value = c(0.000000e+00, 2.758505e-01, 2.572833e-19),
  conf.low = c(4337.13790, -51.67278, 362.91504),
  conf.high = c(4355.8641, 181.0607, 565.0676),
  df = c(14896, 14896, 14896),
  outcome = c("income_fe_t1_self_selection", "income_fe_t1_self_selection", "income_fe_t1_self_selection")
)

stargazer(results,
          title = "RDD fuzzy estimates",
          column.labels = c("Estimate", "Std. Error", "Statistic", "P-value", "95% CI Lower", "95% CI Upper", "DF"),
          align = TRUE,
          dep.var.caption = "Dependent Variable:",
          dep.var.labels = c("Income in T = 1","Self Selection", "Income in T = 1"),
          rownames = FALSE,
          out = "Tables/rdd.tex"
)
