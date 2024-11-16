if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

data <- read_delim("./data/day_01.txt", 
                   delim = "\\n",
                   col_names = "data")

# Part 1 ----
data %>% 
  mutate(
    first_digit = map_chr(
      data,
      ~ str_extract(.x, "[0-9]")
    ),
    last_digit = map_chr(
      data,
      ~ str_extract(paste(rev(strsplit(.x, "")[[1]]), collapse = ""), "[0-9]")
    ),
    combo = as.numeric(paste0(first_digit, last_digit))
  ) %>% 
  pull(combo) %>% 
  sum()


# Part 2 ----

regex <- "([1-9]|one|two|three|four|five|six|seven|eight|nine)"

nums <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

data %>% 
  extract(
    into = "digit_1",
    col = data,
    regex = regex,
    remove = F
  ) %>% 
  extract(
    into = "digit_2",
    col = data,
    regex = paste0(".*", regex),
    remove = F
  ) %>% 
  mutate(
    digit_1 = coalesce(as.numeric(digit_1), match(digit_1, nums)),
    digit_2 = coalesce(as.numeric(digit_2), match(digit_2, nums)),
    combo = as.numeric(paste0(digit_1, digit_2))
  ) %>% 
  pull(combo) %>% 
  sum()