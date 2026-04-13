#clean data, split data, recipe 

library(tidyverse)
library(tidymodels)
library(here)

train <- read_csv(here("data/train.csv"))
test  <- read_csv(here("data/test.csv"))

# Convert outcome to factor
train <- train %>%
  mutate(host_is_superhost = factor(host_is_superhost, levels = c(0,1)))

set.seed(3012)

split <- initial_split(train, prop = 0.8, strata = host_is_superhost)
train_data <- training(split)
val_data   <- testing(split)

rec <- recipe(host_is_superhost ~ ., data = train_data) %>%
  update_role(id, new_role = "id") %>%
  step_rm(id) %>%
  
  # Missing values
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  
  # Encode categories
  step_dummy(all_nominal_predictors()) %>%
  
  # Remove useless cols
  step_zv(all_predictors()) %>%
  
  # Normalize (helps some models)
  step_normalize(all_numeric_predictors())