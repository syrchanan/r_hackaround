if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

# Parsing ----

data <- read_delim("./data/day_02.txt", delim = "\n",
                   col_names = F)

data %>% 
  mutate(round = row_number()) %>% 
  mutate(cubes = str_extract_all(X1, "[0-9]+ [a-z]+")) %>% 
  unnest(cubes) %>% 
  separate(cubes, c("count", "type"), sep = " ", convert = T) -> games_prepped

# Part 1 ----

maximum_opts <- c(red = 12, green = 13, blue = 14)

games_prepped %>% 
  mutate(max = maximum_opts[type]) %>% 
  group_by(round) %>% 
  summarize(valid = !any(count > max)) %>% 
  filter(valid) %>% 
  pull(round) %>% 
  sum()

# Part 2 ----

games_prepped %>% 
  group_by(round, type) %>% 
  summarize(min_needed = max(count)) %>% 
  summarise(power = prod(min_needed)) %>% 
  pull() %>% sum()
