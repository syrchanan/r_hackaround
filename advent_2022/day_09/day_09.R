if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

# Part 1 ####

read_lines("./day_09/input.txt") %>% 
  tibble() %>% 
  separate(1, c("dir", "count")) %>% 
  mutate(count = as.numeric(count)) -> moves

head_pos <- data.frame(move_count = 1, h_x_pos = 0, h_y_pos = 0)

xpos <- 0
ypos <- 0
counter <- 1

for (i in 1:nrow(moves)) {
  
  for (j in 1:as.numeric(moves[i, 2])) {
    
    counter <- counter + 1
    
    if (moves[i,1] == "U") {
      ypos <- ypos + 1
    } else if (moves[i,1] == "D") {
      ypos <- ypos -1
    } else if (moves[i,1] == "R") {
      xpos <- xpos + 1
    } else if (moves[i,1] == "L") {
      xpos <- xpos - 1
    }
    
    new_move <- c(counter, xpos, ypos)
    
    head_pos <- rbind(head_pos, new_move)
    
  }
  
}

tail_pos <- data.frame(move = 1, t_x_pos = 0, t_y_pos = 0)

tcounter <- 1

for (i in 1:(nrow(head_pos)-1)) {
  
  tcounter <- tcounter + 1
  txpos <- 0
  typos <- 0
  
  diffxpos <- head_pos[i+1,2] - tail_pos[i,2]
  diffypos <- head_pos[i+1,3] - tail_pos[i,3]
  
  if (abs(diffxpos) == 2) {
    txpos <- diffxpos/2
    typos <- diffypos
  } else if (abs(diffypos) == 2) {
    txpos <- diffxpos
    typos <- diffypos/2
  }
  
  new_t_move <- c(tcounter, tail_pos[i,2]+txpos, tail_pos[i,3]+typos)
  
  tail_pos <- rbind(tail_pos, new_t_move)
  
}

tail_pos %>% 
  count(t_x_pos, t_y_pos) %>% 
  nrow()

# Part 2 ####

moves <- str_split(readLines("./day_09/input.txt")," ")

rope_move <- function(rope, first, second) {
  vert_diff <- rope[[first]][2] - rope[[second]][2]
  horz_diff <- rope[[first]][1] - rope[[second]][1]
  if (vert_diff < 0) { vert_move <- floor(vert_diff/2) } else { vert_move <- ceiling(vert_diff/2) }
  if (horz_diff < 0) { horz_move <- floor(horz_diff/2) } else { horz_move <- ceiling(horz_diff/2) }
  if (abs(vert_diff) == 2 | abs(horz_diff) == 2) {
    rope[[second]][1] <- rope[[second]][1] + horz_move
    rope[[second]][2] <- rope[[second]][2] + vert_move
  }
  return(rope)
}

unique_tail_locations <- function(moves,rope) {
  tail_locs <- c(paste0(rope[[length(rope)]][1],',',rope[[length(rope)]][2]))
  for (i in 1:length(moves)) {
    dir <- moves[[i]][1]
    dist <- as.integer(moves[[i]][2])
    for (j in 1:dist) {
      if (dir %in% c('U','R')) { direction = 1 } else { direction = -1 }
      if (dir %in% c('U','D')) { rope[[1]][2] <- rope[[1]][2] + direction }
      if (dir %in% c('L','R')) { rope[[1]][1] <- rope[[1]][1] + direction }
      for (k in 1:(length(rope)-1)) {
        rope <- rope_move(rope, k, k+1)
        tail_locs <- c(tail_locs,paste0(rope[[length(rope)]][1],',',rope[[length(rope)]][2]))
      }
    }
  }
  length(unique(tail_locs))
}

rope <- list(c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1),c(1,1))
unique_tail_locations(moves,rope)