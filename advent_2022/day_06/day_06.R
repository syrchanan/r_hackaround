if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "zoo")
set.seed(8675309)

read_lines("./day_06/input.txt") %>% 
  str_split("") -> input

# Part 1 ####

unique_check <- function(letter_input, width) {
  
  check <- length(unique(letter_input))
  
  if (check == width) {
    return(T)
  } else {
    return(F)
  }
  
}

width_1 <- 4

validate_1 <- rollapply(input[[1]], width = width_1, FUN = unique_check, width_1)

which.max(validate_1 == T)+(width_1 - 1)

# Part 2 ####

width_2 <- 14

validate_2 <- rollapply(input[[1]], width = width_2, FUN = unique_check, width_2)

which.max(validate_2 == T)+(width_2 - 1)