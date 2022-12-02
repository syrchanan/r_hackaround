if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

input <- read_lines("./day_02/input.txt")

# Part 1 ####

opp_key <- c("A" = "rock", "B" = "paper", "C" = "scissors")

self_key <- c("X" = "rock", "Y" = "paper", "Z" = "scissors")

input %>% 
  tibble() %>% 
  separate(1, c("opponent", "self"), sep = " ") %>% 
  mutate(opponent = opp_key[opponent],
         self = self_key[self]) %>% 
  mutate(shape_score = case_when(
    self == "rock" ~ 1,
    self == "paper" ~ 2,
    self == "scissors" ~ 3
  )) %>% 
  mutate(outcome = case_when(
    opponent == "scissors" & self == "paper" ~ "lose",
    opponent == "paper" & self == "rock" ~ "lose",
    opponent == "rock" & self == "scissors" ~ "lose",
    
    opponent == "rock" & self == "paper" ~ "win",
    opponent == "scissors" & self == "rock" ~ "win",
    opponent == "paper" & self == "scissors" ~ "win",
    
    T ~ "draw"
  )) %>% 
  mutate(final_score = case_when(
    outcome == "win" ~ 6 + shape_score,
    outcome == "draw" ~ 3 + shape_score,
    outcome == "lose" ~ 0 + shape_score
  )) -> final_tbl

final_tbl %>% 
  pull(final_score) %>% 
  sum() -> part_1_ans

# Part 2 ####

res_key <- c("X" = "lose", "Y" = "draw", "Z" = "win")

win_key <- c("scissors" = "rock", "rock" = "paper", "paper" = "scissors")
lose_key <- c("paper" = "rock", "scissors" = "paper", "rock" = "scissors")

input %>% 
  tibble() %>% 
  separate(1, c("opponent", "result"), sep = " ") %>% 
  mutate(opponent = opp_key[opponent],
         result = res_key[result]) %>% 
  mutate(self = case_when(
    result == "win" ~ win_key[opponent],
    result == "lose" ~ lose_key[opponent],
    T ~ opponent
  )) %>% 
  mutate(shape_score = case_when(
    self == "rock" ~ 1,
    self == "paper" ~ 2,
    self == "scissors" ~ 3
  )) %>% 
  mutate(final_score = case_when(
    result == "win" ~ 6 + shape_score,
    result == "draw" ~ 3 + shape_score,
    result == "lose" ~ 0 + shape_score
  )) -> final_tbl_2

final_tbl_2 %>% 
  pull(final_score) %>% 
  sum() -> part_2_ans
