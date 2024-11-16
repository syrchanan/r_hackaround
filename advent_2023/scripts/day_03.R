if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

# Data ----

data <- read_lines("./data/day_03.txt")

data %>% 
  tibble(x = .) %>% 
  mutate(row = row_number()) %>% 
  mutate(value = str_split(x, "")) %>%
  select(-x) %>%
  unnest(value) %>%
  group_by(row) %>%
  mutate(col = row_number()) %>%
  ungroup() %>% 
  mutate(value = parse_guess(value)) -> clean_data

clean_data %>% 
  mutate(num_flag = str_detect(value, "\\d")) %>% 
  group_by(row) %>% 
  mutate(num_id = paste0(row, ".", cumsum(num_flag != lag(num_flag, default = F)))) %>% 
  group_by(num_id) %>% 
  mutate(full_num = as.numeric(paste0(value, collapse = ""))) %>% 
  ungroup() -> numbers_parsed

# Part 1 ----

adjacent_coords <- expand.grid(row_diff = c(-1:1), col_diff = c(-1:1))

numbers_parsed %>% 
  filter(!is.na(num_id)) %>% 
  crossing(adjacent_coords) %>% 
  mutate(row_adj = row + row_diff,
         col_adj = col + col_diff) %>% 
  inner_join({
    numbers_parsed %>% 
      filter(!is.na(num_id))
  }, join_by(row_adj == row, col_adj == col), suffix = c("", "2")) %>% 
  filter(row != row_adj | col != col_adj) %>% 
  select(-c(row_diff, col_diff)) %>%
  filter(value2 != ".", !num_flag2) %>% 
  arrange(row, col) %>% 
  distinct(num_id, .keep_all = T) %>% 
  pull(full_num) %>% 
  sum(na.rm = T)

# Part 2 ----

numbers_parsed %>% 
  filter(value == "*") %>% 
  crossing(adjacent_coords) %>% 
  mutate(row_adj = row + row_diff,
         col_adj = col + col_diff) %>% 
  inner_join({
    numbers_parsed %>% 
      filter(!is.na(num_id))
  }, join_by(row_adj == row, col_adj == col), suffix = c("", "2")) %>% 
  filter(row != row_adj | col != col_adj) %>% 
  select(-c(row_diff, col_diff)) %>% 
  filter(!is.na(full_num2)) %>% 
  distinct(row, col, num_id2, .keep_all = T) %>% 
  group_by(row, col) %>% 
  summarise(number_gears = n(),
            gear_ratio = prod(full_num2),
            .groups = "drop") %>% 
  filter(number_gears == 2) %>% 
  pull(gear_ratio) %>% sum()
