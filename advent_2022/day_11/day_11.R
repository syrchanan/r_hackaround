if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

read_lines("./day_11/input.txt") %>% 
  str_trim() %>% 
  paste(collapse = "\n") %>% 
  str_split("\n\n") %>% 
  tibble(.[[1]]) %>%
  select(2) %>% 
  rename("raw" = 1) %>% 
  mutate(monkey_num = str_extract(raw, "(?<=Monkey )[0-9]+"),
         items = str_extract(raw, "(?<=items: ).*(?=\\n)") %>% str_split(", "),
         operation = str_extract(raw, "(?<=Operation: ).*(?=\\n)"),
         test_statement = str_extract(raw, "(?<=Test: ).*(?=\\n)"),
         true_statement = str_extract(raw, "(?<=If true: ).*(?=\\n)"),
         false_statement = str_extract(raw, "(?<=If false: ).*")) %>% 
  select(-raw) -> parsed_input

parsed_input %>% 
  mutate(monkey_num = as.numeric(monkey_num),
         true_monkey = as.numeric(str_extract(true_statement, "[0-9]+")),
         false_monkey = as.numeric(str_extract(false_statement, "[0-9]+")),
         test = str_replace(test_statement, "divisible by ", "test = worry %% ")) %>% 
  select(!contains("statement")) -> prepped_input

