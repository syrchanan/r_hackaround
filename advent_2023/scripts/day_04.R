if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

source("./functions.R")

data <- import_advent(4, 2023, read_lines("./session"))

# Part 1 ----

data$parsed_data %>% 
  head(-1) %>%
  separate(x, c("card", "numbers"), ": ") %>% 
  mutate(
    card = row_number(),
    player_nums = str_extract(numbers, "[\\d\\s]+(?= \\|)"),
    winning_nums = str_extract(numbers, "(?<=\\| )[\\d\\s]+")
  ) %>% 
  mutate(
    across(contains("nums"), ~ str_split(str_replace_all(str_trim(.x), "\\s+", " "), pattern = " "))
  ) %>% 
  unnest(player_nums) %>% 
  unnest(winning_nums) %>% 
  mutate(win = as.numeric(player_nums) == as.numeric(winning_nums)) %>%
  group_by(card) %>% 
  summarize(wins = sum(win)) -> wins_by_card


wins_by_card %>% 
  filter(wins > 0) %>% 
  mutate(score = 2^(wins-1)) %>% 
  pull(score) %>% sum()

# Part 2 ----

# test <- tibble(card = 1:6, wins = c(4, 2, 2, 1, 0, 0))

total <- c(1, rep(1, nrow(wins_by_card)-1))

for (i in 1:nrow(wins_by_card)) {
  
  if (wins_by_card$wins[i] == 0) {
    
    next
    
  } else {
    
    cards <- (wins_by_card$card[i]+1):(wins_by_card$card[i]+wins_by_card$wins[i])
    
    print(paste0(">>> Cards to increment: ", paste(cards, collapse = ", ")))
    
    for (j in seq_along(cards)) {
      
      index <- cards[j]
      
      total[index] <- total[index] + total[i]
      
    }
    
  }
  
}

sum(total[1:nrow(wins_by_card)])
