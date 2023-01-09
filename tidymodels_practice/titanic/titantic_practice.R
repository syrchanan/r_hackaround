if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "janitor", "tidymodels")
set.seed(8675309)

train_set <- read_csv("./titanic/train.csv") %>% clean_names()
test_set <- read_csv("./titanic/test.csv") %>% clean_names()

skimr::skim(train_set)

recipe(survived ~ ., train_set) %>% 
  step_rm(name, ticket, age, cabin, embarked) %>% 
  step_zv(all_predictors()) %>% 
  #step_cut(age, breaks = seq(0, 80, by = 10)) %>%
  #step_discretize(fare, num_breaks = 5) %>%
  #step_mutate_at(all_double_predictors(), fn = ~ factor(., ordered = F)) %>%
  #step_mutate_at(cabin, fn = ~ str_extract(., "[a-zA-Z]*")) %>% 
  step_dummy(all_factor_predictors()) %>% 
  step_impute_bag(fare, impute_with = imp_vars(all_predictors())) %>% 
  step_corr(all_numeric_predictors()) -> titanic_rec

linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression") -> mod_log_reg #76% accuracy

workflow() %>% 
  add_model(mod_log_reg) %>% 
  add_recipe(titanic_rec) -> titanic_wflw

titanic_wflw %>% 
  fit(data = train_set) -> fit_wflw

predict(fit_wflw, test_set) %>%
  pull(1) %>% 
  map_dbl(~ round(.x, 0)) %>% 
  bind_cols(test_set$passenger_id) %>% 
  select(2,1) %>% 
  rename("Survived" = 2, "PassengerId" = 1) %>% 
  write_csv("./titanic/predictions.csv")
