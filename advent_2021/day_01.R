

library(tidyverse)

# Read in Data ####

depth <- read_delim("day_01.txt", delim = "\r\n", 
                    escape_backslash = TRUE, col_names = c("depth"))

#sample <- tibble("depth" = c(199,200,208,210,200,207,240,269,260,263))

# Part 1 ####

depth %>% 
  mutate(depth_increased = case_when(
    depth > lag(depth) ~ 1,
    depth <= lag(depth) ~ 0,
  )) %>% 
  filter(depth_increased == 1) %>% 
  nrow()

# Part 2 ####

depth %>%
  mutate(rolling_sum = depth + lag(depth) + lag(depth, n = 2)) %>%
  mutate(depth_increased = case_when(
    rolling_sum > lag(rolling_sum) ~ 1,
    rolling_sum <= lag(rolling_sum) ~ 0,
  )) %>% 
  filter(depth_increased == 1) %>% 
  nrow()

