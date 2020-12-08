install.packages(c("tidyverse","janitor", "cluster","fpc"))

# Data cleaning

data <- read.csv("data/heart_data.csv") %>%
  # Convert data.frame object to a tibble, which prints nicer
  tibble::as_tibble() %>%
  
  # Clean the variable names, to make them easier to work with
  janitor::clean_names() %>%
  
  # Create a new heart_disease variable, which is class - 1
  # Change other binary variables to have values: 0,1
  # Now for all the variables below: 0 <= var <= 1
  dplyr::mutate(heart_disease = class - 1) %>%
  dplyr::mutate(sex = sex - 1) %>%
  dplyr::mutate(fasting_blood_sugar = fasting_blood_sugar - 1) %>%
  dplyr::mutate(exercise_induced = exercise_induced - 1) %>%
  
  # Remove no longer needed variables
  dplyr::select(-c(class)) %>%
  
  # Convert binary and ordered variables to factors, so R knows to treat them as
  # binary/ordered
  dplyr::mutate(dplyr::across(
    c(sex, fasting_blood_sugar, exercise_induced, heart_disease),
    factor
  )) %>%
  dplyr::mutate(dplyr::across(c(slope), factor, ordered = TRUE))


# Sanity check ---- Sex should most definitely be associated with heart disease,
# though not clear which gender is which from the data documentation

mylogit <- glm(heart_disease ~ sex, data = data, family = "binomial")

# See results of model
summary(mylogit)

# Convert to an odds ratio
# OR = ~4 when moving from sex = 1 to sex = 2
exp(coef(mylogit))

# Unsupervised learning
# Taken from this answer: https://stats.stackexchange.com/a/164694

# Remove heart disease from the dataset, as this is the labelled outcome
data_no_outcome <- data[,-10]

# Compute Gower's distance, which can have binary and continuous
g.dist = daisy(data_no_outcome, metric="gower")

# Cluster based on this distance, which seems to recommend 2 clusters
pc = pamk(g.dist, criterion="asw")

# Get vector containing cluster info by patient
cluster_vector <- pc$pamobject$clustering
