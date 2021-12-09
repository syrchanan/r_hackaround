
library(tidyverse)

paths <- tibble(read_lines("day_05.txt"))

# Part 1 ####

paths %>% 
  rename("full_path" = `read_lines("day_05.txt")`) %>%
  separate(col = full_path, into = c("x1,y1", "x2,y2"), sep = " -> ") %>%
  separate(col = `x1,y1`, into = c("x1", "y1")) %>%
  separate(col = `x2,y2`, into = c("x2", "y2")) %>% 
  mutate(across(c(1:4), as.numeric)) %>% 
  mutate(x_diff = x2-x1,
         y_diff = y2-y1) -> paths_dim

dim <- max(paths_dim)

map_init <- matrix(data = 0, nrow = dim+1, ncol = dim+1)

for (i in 1:nrow(paths_dim)) {
  
  x1 <- paths_dim$x1[i] + 1
  x2 <- paths_dim$x2[i] + 1
  y1 <- paths_dim$y1[i] + 1
  y2 <- paths_dim$y2[i] + 1
  
  if (y1 == y2 | x1 == x2) {
    map_init[y1:y2, x1:x2] <- map_init[y1:y2, x1:x2] + 1
  }
  
}

sum(map_init >= 2)

# Part 2 ####

map_init <- matrix(data = 0, nrow = dim+1, ncol = dim+1)

for (i in 1:nrow(paths_dim)) {
  
  x1 <- paths_dim$x1[i] + 1
  x2 <- paths_dim$x2[i] + 1
  y1 <- paths_dim$y1[i] + 1
  y2 <- paths_dim$y2[i] + 1
  
  if (y1 == y2 | x1 == x2) {
    map_init[y1:y2, x1:x2] <- map_init[y1:y2, x1:x2] + 1
  } else {
    x_len <- seq(x1, x2)
    y_len <- seq(y1, y2)
    
    for (j in 1:length(x_len)) {
      map_init[y_len[j], x_len[j]] <- map_init[y_len[j], x_len[j]] + 1
    }
  }
}

sum(map_init >= 2)

