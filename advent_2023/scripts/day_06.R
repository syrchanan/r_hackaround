if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "janitor")
set.seed(8675309)

source("./functions.R")

data <- import_advent(6, 2023, read_lines("./session"))

# Part 1 ----

data$parsed_data %>% 
  head(-1) %>% 
  mutate(x = str_replace_all(x, regex("\\s+"), " ")) %>% 
  mutate(values = map_vec(x,
                          ~ str_extract_all(.x, "(\\d+)"))) %>% 
  unnest(values) %>% 
  group_by(x) %>% 
  mutate(game = row_number()) %>% 
  ungroup() %>% 
  mutate(x = str_extract(x, "[a-zA-Z]+(?=:)")) %>% 
  pivot_wider(names_from = x, values_from = values) %>% 
  clean_names() %>% 
  mutate(wait = map_vec(time,
                        ~list(1:.x))) %>% 
  unnest(wait) %>% 
  mutate(across(c(time, distance), as.numeric)) %>% 
  mutate(rem_time = time - wait,
         total_dist = wait*rem_time,
         win = total_dist > distance) %>% 
  group_by(game) %>% 
  summarize(total_wins = sum(win)) %>% 
  ungroup() %>% 
  pull(total_wins) %>% 
  prod(.)

# Part 2 ----

data$parsed_data %>% 
  head(-1) %>% 
  mutate(x = str_replace_all(x, regex("\\s+"), "")) %>% 
  mutate(values = map_vec(x,
                          ~ as.numeric(str_extract_all(.x, "(\\d+)")))) %>% 
  unnest(values) %>%
  mutate(x = str_extract(x, "[a-zA-Z]+(?=:)")) %>% 
  pivot_wider(names_from = x, values_from = values) %>% 
  clean_names() %>% 
  mutate(wait = map_vec(time,
                        ~list(1:.x))) %>% 
  unnest(wait) %>% 
  mutate(across(c(time, distance), as.numeric)) %>% 
  mutate(rem_time = time - wait,
         total_dist = wait*rem_time,
         win = total_dist > distance) %>% 
  summarize(sum(win))
