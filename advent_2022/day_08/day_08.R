if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

# Load and Clean ####

input <- read_lines("./day_08/input.txt")

sep_input <- flatten(map(input, str_split, "")) %>% map(as.integer)

tree_map <- tibble(as.data.frame(do.call(rbind, sep_input)))

# Part 1 ####

is_visible <- function(i, j) {
  
  tree_height <- tree_map[i,j] %>% pull(1)
  
  if (any(i == 1, i == nrow(tree_map), j == 1, j == ncol(tree_map))) {
    return(T)
  }
  
  if (
    all(
      any(tree_map[1:(i-1), j] >= tree_height), #above
      any(tree_map[(i+1):nrow(tree_map), j] >= tree_height), #below
      any(tree_map[i, 1:(j-1)] >= tree_height), #left
      any(tree_map[i, (j+1):ncol(tree_map)] >= tree_height) #right
    )
  ) {
    return(F)
  } else {
    return(T)
  }
  
}

visible_tree_map <- matrix(nrow = nrow(tree_map), ncol = ncol(tree_map))

for (i in 1:nrow(tree_map)) {
  for (j in 1:ncol(tree_map)) {
    visible_tree_map[i,j] <- is_visible(i,j)
  }
}

sum(visible_tree_map)

# Part 2 ####

scenery <- function(i, j) {
  
  tree_height <- tree_map[i, j] %>% pull(1)
  
  if (any(i == 1, i == nrow(tree_map), j == 1, j == ncol(tree_map))) {
    return(0)
  }
  
  scores <- c(min(which(tree_map[(i-1):1,j] >= tree_height)[1],
                  length(tree_map[(i-1):1]),na.rm = T), #above 
              
              min(which(tree_map[(i+1):nrow(tree_map),j] >= tree_height)[1],
                  length(tree_map[(i+1):nrow(tree_map)]),na.rm = T), #below
              
              min(which(tree_map[i,(j-1):1] >= tree_height)[1],
                  length(tree_map[(j-1):1]),na.rm = T), #left
              
              min(which(tree_map[i,(j+1):ncol(tree_map)] >= tree_height)[1],
                  length(tree_map[(j+1):ncol(tree_map)]),na.rm = T)) # right
  
  score <- prod(scores)
  
  return(score)
  
}

scenic_tree_map <- matrix(nrow = nrow(tree_map), ncol = ncol(tree_map))

for (i in 1:nrow(tree_map)) {
  for (j in 1:ncol(tree_map))
    scenic_tree_map[i, j] <- scenery(i,j)
}

max(scenic_tree_map)
