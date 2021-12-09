

library(tidyverse)

# Read in Data ####

directions <- read_delim("day_02.txt", delim = "\r\n", 
                    escape_backslash = TRUE, col_names = c("movement"))

directions <- tibble("movement" = c("forward 5","down 5","forward 8",
                                    "up 3","down 8","forward 2"))

# Part 1 ####

directions %>% 
  separate(movement, c("direction", "distance")) %>% 
  mutate(distance = as.numeric(distance)) -> directions_split

depth <- 0
horizontal <- 0

for (i in 1:nrow(directions_split)) {
  
  if (directions_split$direction[i] == "forward") {
    horizontal <- horizontal + directions_split$distance[i]
  } else if (directions_split$direction[i] == "up") {
    depth <- depth - directions_split$distance[i]
  } else if (directions_split$direction[i] == "down") {
    depth <- depth + directions_split$distance[i]
  }
  
}

depth*horizontal

# Part 2 ####

depth <- 0
horizontal <- 0
aim <- 0

for (i in 1:nrow(directions_split)) {
  
  if (directions_split$direction[i] == "forward") {
    horizontal <- horizontal + directions_split$distance[i]
    depth <- depth + (directions_split$distance[i]*aim)
  } else if (directions_split$direction[i] == "up") {
    aim <- aim - directions_split$distance[i]
  } else if (directions_split$direction[i] == "down") {
    aim <- aim + directions_split$distance[i]
  }
  
}

depth*horizontal
