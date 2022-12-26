if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

# Parsing data ####

moves <- read_lines("./day_05/input.txt", skip = 10)

tibble(moves) %>% 
  mutate(moves = str_remove_all(moves, "[:alpha:]+\\s")) %>% 
  separate(moves, c("move", "from", "to")) %>% 
  mutate(across(everything(), as.numeric)) -> moves_tbl

stacks <- read_lines("./day_05/input.txt", n_max = 9)

num_stacks <- length(str_locate_all(max(stacks), "\\[")[[1]])/2

#init stacks
for (i in 1:num_stacks) {
  assign(paste0("stack_", i), c())
}

split_stacks <- str_split(stacks, "")

all_rows <- matrix(nrow = num_stacks, ncol = num_stacks)

for (i in 1:length(split_stacks)) {
  
  temp <- list(split_stacks[[i]])
  
  row <- temp[[1]][seq(2, length(temp[[1]]), 4)]

  all_rows[i,] <- row 
  
}

all_rows <- all_rows[seq(1, num_stacks-1),]

for (i in 1:ncol(all_rows)) {
  
  assign(paste0("stack_", i), all_rows[,i])
  
}

# Part 1 ####

move_boxes <- function(count, from, to) {
  
  temp_from <- get(paste0("stack_", from))
  temp_to <- get(paste0("stack_", to))
 
  moving <- temp_from[seq(1, count)]
  
  temp_from <- temp_from[!temp_from %in% moving]
  
  temp_to <- c(moving, temp_to)
  
  assign(paste0("stack_", from), temp_from)
  assign(paste0("stack_", to), temp_to)
  
}
