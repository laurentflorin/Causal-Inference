###### Data Generation

# This file is used to generate the artificial dataset


#### Packages
library(dplyr)
library(stats)
library(mc2d)
library(extraDistr)
#### functions

# function for a truncated normal distribution
rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  if (a > b) stop('Error: Truncation range is empty');
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd));
  qnorm(U, mean, sd); }

rdu<-function(n,min,max) sample(min:max,n,replace=T) # descrete random uniform distribution

# set seed for reproducability
set.seed(123)

###### Start Code 

n = 150000 #size of data set


# first generate the covariates
# I used different distributions than uniform to have them more realistic,
# but we can also set them all to uniform 
data = tibble(
  income = rtruncnorm(n, mean = 2400, sd = 2000, a = 0), #outcome 
  age = rpert(n, min = 18, mode = 30, max = 65, shape = 3), #covariate
  motivation = runif(n),
  gender = rdu(n,0,1),
  marital_status = rbinom(n, size = 1, prob = 0.3), # probability of being married 30%
  education_level = rdu(n,0,5),
  zipcode = rbinom(n, size = 1, prob = 0.3), #either in Zurich or not, Zurich 30%
  country_origin = rbinom(n, size = 1, prob = 0.8), #either schengen or rest of world, most probably from schengen
  social_benefits = rbinom(n, size = 1, prob = 0.1),
  course_or_not = rbinom(n, size = 1, prob = 0.3),
  years_in_ch = rdu(n, 0,5),
  work_percentage = rbbinom(n, 10, alpha = 3, beta = 1)/10, #most will work close to 100%
  income_fe = ifelse(work_percentage == 0, income, income/work_percentage),
  id = 1:n)
#no_of_children 
#age of youngest child
#continue_taking_course



# generate the outcome
delta <- 400 # set the treatment effect
v <- rnorm(n,0,50) #error term

# in an experiment the participants would be assigned to take the course or not randomly
d_assignment <- rbinom(n, size = 1, prob = 0.5)

#self selection, self selection into taking german course depends on covariates
d_star <- -200 + 50 * data$zipcode - 0.5*data$age + 20*data$education_level + 5*data$education_level^2  - 10*data$work_percentage + 70*motivation  + rnorm(n, 0, 20) 
hist(d_star)
d_self_selection <- d_star
for (i in 1:n){
  if (d_star[i] > 0){
    d_self_selection[i] <- 1
  } else {d_self_selection[i] <- 0 }
}

# Outcone Variable

income_fe_t1_assignment = mean(data$income_fe) + 0.05*(data$income_fe - mean(data$income_fe))  + 4*data$age + 0.05*data$age^2 + 200*data$gender + 100*data$marital_status - 200*data$social_benefits + 30*data$education_level + 3*data$years_in_ch + 120*data$motivation + d_assignment*delta + v
hist(income_fe_t1_assignment, breaks = 100)  

income_fe_t1_self_selection = mean(data$income_fe) + 0.05*(data$income_fe - mean(data$income_fe))  + 4*data$age + 0.05*data$age^2 + 200*data$gender + 100*data$marital_status - 200*data$social_benefits + 30*data$education_level + 3*data$years_in_ch + 120*data$motivation + d_self_selection*delta + v
hist(income_fe_t1_self_selection, breaks = 100)  

outcome <- tibble(income_fe_t1_assignment = income_fe_t1_assignment,
                  income_fe_t1_self_selection = income_fe_t1_self_selection,
                  d_assignment = d_assignment,
                  d_self_selection = d_self_selection,
                  id = 1:n)


full_data <- data %>% left_join(outcome, by = "id")

# summary statistics for assignment and self selection 
full_data %>% group_by(d_assignment) %>% select(income_fe, age, gender, marital_status, social_benefits, education_level, years_in_ch, motivation) %>% summarise_all(mean)
full_data %>% group_by(d_self_selection) %>% select(income_fe, age, gender, marital_status, social_benefits, education_level, years_in_ch, motivation) %>% summarise_all(mean)
