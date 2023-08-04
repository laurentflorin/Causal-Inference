###### Data Generation

# This file is used to generate the artificial dataset
# It does what is describet in Exercise 2 and 3a

rm(list=ls()); gc()
#### Packages --------
library(dplyr)
library(stats)
library(mc2d)
library(extraDistr)
library(ggplot2)

#### Functions --------

# function for a truncated normal distribution
rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
  if (a > b) stop("Error: Truncation range is empty")
  U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd))
  qnorm(U, mean, sd)
  }

# descrete random uniform distribution
rdu <- function(n, min, max) sample(min:max, n, replace = TRUE)

# set seed for reproducability
set.seed(123)

my_colors <- RColorBrewer::brewer.pal(4, "Set1")

#### Set main parameters--------

n <- 150000 #size of data set


#### The Covariates --------
# I used different distributions than uniform to have them more realistic,
# but we can also set them all to uniform
data <- tibble(
  age = round(rpert(n, min = 18, mode = 30, max = 65, shape = 3),0), # age
  motivation = runif(n),
  #continue_taking_course = runif(n), #probability of continuing course once they start the course
  gender = rbinom(n, size = 1, prob = 0.5),
  distance = runif(n, 0, 1),
  education_level = rdu(n, 0, 5),
  #zipcode = rbinom(n, size = 1, prob = 0.3), # either in Zurich or not, Zurich 30%
  #country_origin = rbinom(n, size = 1, prob = 0.8), # either schengen or rest of world, most probably from schengen
  #social_benefits = rbinom(n, size = 1, prob = 0.1),
  #course_or_not = rbinom(n, size = 1, prob = 0.3), # taken a german course before
  years_in_ch = rdu(n, 0, 5),
  work_percentage = rbbinom(n, 10, alpha = 3, beta = 1) / 10, # most will work close to 100%
  #income_t0 = rtruncnorm(n, mean = 2400, sd = 2000, a = 0), # income before treatment
  #income_fe_t0 = ifelse(work_percentage == 0, income_t0, income_t0 / work_percentage), # full time equivalent of income
  id = 1:n,
  has_children = rbinom(n, size = 1, prob = 0.7), # If person has children with 70% of working population already having kids
  children_german_primary = ifelse(has_children == 1, rbinom(n, size = 1, prob = 0.3), 0), # Whether they have children in German-speaking primary school
  children_english_primary = ifelse(has_children == 1 & children_german_primary == 0, rbinom(n, size = 1, prob = 0.3), 0), # Whether those without children in German primary have children in another primary school
  no_of_children = ifelse(has_children == 1, round(rtruncnorm(n, mean = 2, sd = 1, a = 0, b = 5), 0), 0), # Number of children,
  swiss_working_culture = rbinom(n, size = 1, prob = 0.6)
)
#age of youngest child
#### The Outcome --------

u_0 <- rnorm(n, 0, 50) #error term U_i(0)
u_1 <- rnorm(n, 0, 50) #error term U_i(1)

# Equation (4): Treatment Effect
delta <- 400 + u_1 - u_0

# Selection Rule 1
# Random assignment to treatment with a probability of 50%
d_assignment <- rbinom(n, size = 1, prob = 0.5)

# Self selection, self selection into taking German course depends on covariates: equation (2) and (3) of the assignment

#50 * data$zipcode -> if in zurich more likely to take te free course
# -0.5*data$age -> older people less likely to learn new language
# 20*data$education_level + 5*data$education_level^2  -> higher education more likely to take it?
#10*data$work_percentage -> more free time more likely to take it
#70*data$motivation -> generally more motivated, more likely to take it
#10*data$social_benefits ->  more time to take the course more likely to take it

# Selection Rule 2
# Equation (2)
d_star <- -50 -  data$age - data$education_level - 5 * data$work_percentage + 90 * data$motivation - 20 * data$distance + 40 * data$children_german_primary + rnorm(n, 0, 20)

# Histogram
ggplot() + 
geom_histogram(aes(d_star),binwidth=10, colour = "white", fill = my_colors[2]) +
scale_x_continuous(name="Value") +
scale_y_continuous(name="Frequency") +
theme_bw(base_size = 16)

# Check if data directory already exists otherwise create
dir.create(file.path("Graphs"), showWarnings = FALSE)

ggsave("Graphs/d_star_distribution.png", plot = last_plot(), width = 8, height = 6)

# Equation (3)
d_self_selection <- d_star
for (i in 1:n){
  if ((d_star[i] > 0 & data$distance[i] <= 0.4) |(d_star[i] > 0 & data$distance[i] > 0.4 & data$motivation[i] > 0.85)) {
    d_self_selection[i] <- 1
  } else {
    d_self_selection[i] <- 0
    }
}
# Histogram
ggplot() + 
geom_histogram(aes(d_self_selection), colour = "white", fill = my_colors[2]) +
scale_x_continuous(name="Value") +
scale_y_continuous(name="Frequency") +
theme_bw(base_size = 16)

ggsave("Graphs/d_self_distribution.png", plot = last_plot(), width = 8, height = 6)



# Define who is in fact treated
# in this case, who takes the german course
# those with a very low d_star (< -75) are Never-Takers
# those with a positive d_star are Always-Takers
# we assume monotonicity and no defiers
#d_treated <- ifelse(d_star > -75 & d_assignment == 1, ifelse(d_self_selection == 1, 1, 0), 0)

# Outcome Variable ------
# Outcome is the full time equivalent income at T+1 afer treatment
# The treatment is receiving a voucher for a free german course

# The outcome depends on the observable covariates as well as the unobservable variable motivation:

# mean(data$income_fe) -> assumption, with out treatment at T+1 the average income is identical to T
# 0.05*(data$income_fe - mean(data$income_fe)) -> if they earned more at T they likely earn more at T+1 (not sure if necessary)
# 4*data$age + 0.05*data$age^2 -> higher age normally means higher sallary
# 200*data$gender -> if we "want" gender to have an influence
# 100*data$marital_status
# - 200*data$social_benefits -> people who recieved social benefits probably earn less
# 30*data$education_level -> higher eductation likely higher income
# 3*data$years_in_ch -> longer in ch likely higher income
# 120*data$motivation -> higher motivation likely higher income independent of treatment
# 160*data$continue_taking_course -> continues with course likely higher income independent of treatment
# 40*data$arrival_since^2 -> the longer a person is in Switzerland the higher the income
# 80*data$arrival_since *d_self_selection -> the treatment effect is larger for those longer in Switzerland


# d_assignment*delta -> treatment effect
# v -> error term

# Equation (1)
income_fe_t1_assignment <- 4000  + 4 * data$age + 0.05 * data$age^2  + 30 * data$education_level + 3 * data$years_in_ch + 120 * data$motivation  + d_assignment * delta + u_0

# Histogram
ggplot() + 
geom_histogram(aes(income_fe_t1_assignment),binwidth=10, colour = "white", fill = my_colors[2]) +
scale_x_continuous(name="Income") +
scale_y_continuous(name="Frequency") +
theme_bw(base_size = 16)
ggsave("Graphs/income_t1_assignment_distribution.png", plot = last_plot(), width = 8, height = 6)

#hist(income_fe_t1_assignment, breaks = 100)

income_fe_t1_self_selection <- 4000  + 4 * data$age + 0.05 * data$age^2  + 30 * data$education_level + 3 * data$years_in_ch + 120 * data$motivation + d_self_selection * delta + u_0

# Histogram
ggplot() + 
geom_histogram(aes(income_fe_t1_self_selection),binwidth=10, colour = "white", fill = my_colors[2]) +
scale_x_continuous(name="Income") +
scale_y_continuous(name="Frequency") +
theme_bw(base_size = 16)
ggsave("Graphs/income_t1_self_distribution.png", plot = last_plot(), width = 8, height = 6)

#hist(income_fe_t1_self_selection, breaks = 100)


income_fe_t0 <- 4000  + 4 * data$age + 0.05 * data$age^2  + 30 * data$education_level + 3 * data$years_in_ch + 120 * data$motivation + u_0

# Histogram
ggplot() + 
geom_histogram(aes(income_fe_t0),binwidth=10, colour = "white", fill = my_colors[2]) +
scale_x_continuous(name="Income") +
scale_y_continuous(name="Frequency") +
theme_bw(base_size = 16)
ggsave("Graphs/income_t0.png", plot = last_plot(), width = 8, height = 6)

#hist(income_fe_t0, breaks = 100)


outcome <- tibble(income_fe_t1_assignment = income_fe_t1_assignment,
                  income_fe_t1_self_selection = income_fe_t1_self_selection,
                  income_fe_t0 = income_fe_t0,
                  d_assignment = d_assignment,
                  d_self_selection = d_self_selection,
                  id = 1:n)


full_data <- data %>% left_join(outcome, by = "id")

# Summary Statistics ------
# For assignment and self selection

full_data %>% group_by(d_assignment) %>% select(d_assignment, age, gender, education_level, years_in_ch, motivation,income_fe_t0, income_fe_t1_assignment) %>% summarise_all(mean)
full_data %>% count(d_assignment)
full_data %>% group_by(d_self_selection) %>% select(d_self_selection, age, gender, education_level, years_in_ch, motivation,income_fe_t0, income_fe_t1_self_selection) %>% summarise_all(mean)
full_data %>% count(d_self_selection)

# Histogram
ggplot(full_data, aes(income_fe_t1_self_selection, y=..density.., fill = factor(d_self_selection))) + 
geom_histogram(position = "identity",bins = 30, colour = "white", alpha = 0.8) +
scale_x_continuous(name="Income") +
scale_y_continuous(name="Density") +
theme_bw(base_size = 16) +
scale_fill_manual(values = my_colors[1:2], name = "Self-Selection", labels = c("No", "Yes"))

ggsave("Graphs/Income_By_Self_Selection.png", plot = last_plot(), width = 8, height = 6)

#hist(full_data$income_fe_t1_self_selection[d_self_selection == 1], freq = FALSE)
#hist(full_data$income_fe_t1_self_selection[d_self_selection == 0], freq = FALSE, col = 2, add = TRUE)

# Export data as CSV -------
# Check if data directory already exists otherwise create
dir.create(file.path("Data"), showWarnings = FALSE)

write.csv2(full_data, file = "Data/dataset.csv")




