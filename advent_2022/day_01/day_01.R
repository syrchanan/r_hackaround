if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

elves <- as.numeric(read_lines("day_01/day_01_input.txt"))

temp_list <- c()
total_list <- c()

for (i in seq_along(elves)) {
  
  if (is.na(elves[i])) {
    total <- sum(temp_list)
    total_list <- append(total_list, total)
    temp_list <- c()
  } else {
    temp_list <- append(temp_list, elves[i])
  }
  
}

max(total_list)

total_list %>% 
  tibble() %>% 
  arrange(desc(total_list)) %>% 
  slice(1:3) %>% 
  pull(1) %>% 
  sum()
