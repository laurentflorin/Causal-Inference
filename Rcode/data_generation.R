###### Data Generation

# This file is used to generate the artificial dataset
# It does what is describet in Exercise 2 and 3a


#### Packages --------
library(dplyr)
library(stats)
library(mc2d)
library(extraDistr)

#### Functions --------

# function for a truncated normal distribution
rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  if (a > b) stop('Error: Truncation range is empty');
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd));
  qnorm(U, mean, sd); }

rdu<-function(n,min,max) sample(min:max,n,replace=T) # descrete random uniform distribution

# set seed for reproducability
set.seed(123)

#### Set main parameters--------

n = 150000 #size of data set


#### The Covariates --------
# I used different distributions than uniform to have them more realistic,
# but we can also set them all to uniform 
# I think we dont need income and income_fe in the covariate matrix as it is our outcome -> we are not interested how much they earned before the treatment only how much they earn afeter compared to those who didnt recive the treatment
data = tibble(
  #income = rtruncnorm(n, mean = 2400, sd = 2000, a = 0), #income before treatment 
  age = rpert(n, min = 18, mode = 30, max = 65, shape = 3), #covariate
  motivation = runif(n),
  gender = rdu(n,0,1),
  marital_status = rbinom(n, size = 1, prob = 0.3), # probability of being married 30%
  education_level = rdu(n,0,5),
  zipcode = rbinom(n, size = 1, prob = 0.3), #either in Zurich or not, Zurich 30%
  country_origin = rbinom(n, size = 1, prob = 0.8), #either schengen or rest of world, most probably from schengen
  social_benefits = rbinom(n, size = 1, prob = 0.1),
  course_or_not = rbinom(n, size = 1, prob = 0.3), #taken a german course before
  years_in_ch = rdu(n, 0,5),
  work_percentage = rbbinom(n, 10, alpha = 3, beta = 1)/10, #most will work close to 100%
  #income_fe = ifelse(work_percentage == 0, income, income/work_percentage), #full time equivalent of income
  id = 1:n)
#no_of_children 
#age of youngest child
#continue_taking_course



#### The Outcome --------

u_0 <- rnorm(n,0,50) #error term U_i(0)
u_1 <- rnorm(n,0,50) #error term U_i(1)

delta <- 400 + u_1 - u_0  # set the treatment effect: equation (4) of the assignment


# Random assignment to treatment with a probability of 50%
d_assignment <- rbinom(n, size = 1, prob = 0.5)

#self selection, self selection into taking German course depends on covariates: equation (2) and (3) of the assignment

#50 * data$zipcode -> if in zurich more likely to take te free course
# -0.5*data$age -> older people less likely to learn new language 
# 20*data$education_level + 5*data$education_level^2  -> higher education more likely to take it?
#10*data$work_percentage -> more free time more likely to take it 
#70*data$motivation -> generally more motivated, more likely to take it
#10*data$social_benefits ->  more time to take the course more likely to take it

d_star <- -200 + 50 * data$zipcode - 0.5*data$age + 20*data$education_level + 5*data$education_level^2  - 10*data$work_percentage + 70*data$motivation + 10*data$social_benefits + rnorm(n, 0, 20) 
hist(d_star)
d_self_selection <- d_star
for (i in 1:n){
  if (d_star[i] > 0){
    d_self_selection[i] <- 1
  } else {d_self_selection[i] <- 0 }
}
hist(d_self_selection)

# Outcone Variable ------
# Outcome is the full time equilvalent income at T+1 afer treatment (either receiving a voucher for a free german course or taking the free germant course we have to decide)
# It depends on the observable covariates as well as the unobservable variable motivation:
# equation (1) of the assignment

# mean(data$income_fe) -> assumption, with out treatment at T+1 the average income is identical to T
# 0.05*(data$income_fe - mean(data$income_fe)) -> if they earned more at T they likely earn more at T+1 (not sure if necessary)
# 4*data$age + 0.05*data$age^2 -> higher age normally means higher sallary 
# 200*data$gender -> if we "want" gender to have an influence 
# 100*data$marital_status 
# - 200*data$social_benefits -> people who recieved social benefits probably earn less
# 30*data$education_level -> higher eductation likely higher income
# 3*data$years_in_ch -> longer in ch likely higher income
# 120*data$motivation -> higher motivation likely higher income independent of treatment
# d_assignment*delta -> treatment effect
# v -> error term

income_fe_t1_assignment = 4000  + 4*data$age + 0.05*data$age^2 + 200*data$gender + 100*data$marital_status - 200*data$social_benefits + 30*data$education_level + 3*data$years_in_ch + 120*data$motivation + d_assignment*delta + u_0
hist(income_fe_t1_assignment, breaks = 100)  

income_fe_t1_self_selection = 4000 + 4*data$age + 0.05*data$age^2 + 200*data$gender + 100*data$marital_status - 200*data$social_benefits + 30*data$education_level + 3*data$years_in_ch + 120*data$motivation + d_self_selection*delta + u_0
hist(income_fe_t1_self_selection, breaks = 100)  

outcome <- tibble(income_fe_t1_assignment = income_fe_t1_assignment,
                  income_fe_t1_self_selection = income_fe_t1_self_selection,
                  d_assignment = d_assignment,
                  d_self_selection = d_self_selection,
                  id = 1:n)


full_data <- data %>% left_join(outcome, by = "id")

# summary statistics for assignment and self selection 

full_data %>% group_by(d_assignment) %>% select( age, gender, marital_status, social_benefits, education_level, years_in_ch, motivation, income_fe_t1_assignment) %>% summarise_all(mean)
full_data %>% group_by(d_self_selection) %>% select(age, gender, marital_status, social_benefits, education_level, years_in_ch, motivation, income_fe_t1_self_selection) %>% summarise_all(mean)

hist(full_data$income_fe_t1_self_selection[d_self_selection == 1 ], freq = F)
hist(full_data$income_fe_t1_self_selection[d_self_selection == 0 ],freq = F, col = 2, add = T)

#Save Data as CSV
write.csv2(full_data,file = "Data/dataset.csv")
