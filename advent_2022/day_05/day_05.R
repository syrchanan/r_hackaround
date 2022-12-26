if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

# Loading all data and parsing ####

input_all <- read_lines("./day_05/input.txt")

input_all %>% 
  tail_while(~ .x != "") %>% 
  str_extract_all("\\d+") %>% 
  map(as.integer) -> moves_list

read_fwf("./day_05/input.txt", 
        n_max = length(input_all) - length(moves_list) - 2,
        col_types = "c") %>% 
  mutate(across(everything(), ~ str_extract(.x, "[:alpha:]"))) %>% 
  as.list() %>% 
  map(discard, is.na) %>% 
  map(rev) -> stacks

# Part 1 ####

for (move in moves_list) {
  
  num <- move[1]
  from <- move[2]
  to <- move[3]
  
  boxes <- rev(tail(stacks[[from]], num))
  
  stacks[[to]] <- append(stacks[[to]], boxes)
  stacks[[from]] <- head(stacks[[from]], -1 * num)
}

stacks %>% 
  map(~ tail(.x, 1)) %>% 
  str_c(collapse = "")


# Part 2 ####

for (move in moves_list) {
  
  num <- move[1]
  from <- move[2]
  to <- move[3]
  
  boxes <- tail(stacks[[from]], num)
  
  stacks[[to]] <- append(stacks[[to]], boxes)
  stacks[[from]] <- head(stacks[[from]], -1 * num)
}

stacks %>% 
  map(~ tail(.x, 1)) %>% 
  str_c(collapse = "")
