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


# Linear Reg ####

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

# XGBoost ####

train_set$survived <- factor(train_set$survived)

train_split <- initial_split(train_set, prop = 0.75)

recipe(survived ~ ., training(train_split)) %>% 
  step_rm(name, ticket, age, cabin, embarked) %>% 
  step_zv(all_predictors()) %>% 
  step_dummy(all_factor_predictors()) %>% 
  step_impute_bag(fare, impute_with = imp_vars(all_predictors())) %>% 
  step_corr(all_numeric_predictors()) -> titanic_rec

boost_tree(learn_rate = tune(), tree_depth = tune(), 
           loss_reduction = tune(), trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") -> mod_xgb

workflow() %>% 
  add_model(mod_xgb) %>% 
  add_recipe(titanic_rec) -> titanic_wflw

cfold <- vfold_cv(training(train_split))

tune_res <- titanic_wflw %>% 
  tune_grid(resamples = cfold,
            grid = expand.grid("trees" = 15:20,
                               "learn_rate" = c(0.3, 0.2, 0.1, 0.01, 0.001, 0.0001),
                               "loss_reduction" = 1:3,
                               "tree_depth" = 4:9))

tune_res %>%
  select_best("accuracy")

tune_res %>%
  select_best("accuracy") %>% 
  finalize_workflow(titanic_wflw, .) %>% 
  last_fit(train_split) %>% 
  collect_predictions() %>% 
  conf_mat(survived, .pred_class)



# To extract predicitons ####

predict(fit_wflw, test_set) %>%
  pull(1) %>%
  #map_dbl(~ round(.x, 0)) %>% 
  bind_cols(test_set$passenger_id) %>% 
  select(2,1) %>% 
  rename("Survived" = 2, "PassengerId" = 1) %>% 
  write_csv("./titanic/predictions.csv")
