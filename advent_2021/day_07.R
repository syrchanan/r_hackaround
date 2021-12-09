
library(tidyverse)

pos <- c(16,1,2,0,4,2,7,1,2,14)
pos <- as.numeric(str_split(read_lines("day_07.txt"), ",")[[1]])

# Part 1 ####

target <- median(pos)

distance <- abs(pos-target)

sum(distance)

# Part 2 ####

# going to have to look for every possibility now, not just median
# since cost has become exponential

max_pos <- max(pos)
fuel_burn <- rep(0,max_pos)

for (position in 1:max_pos) {
  for (i in 1:length(pos)) {
    distance <- abs(pos[i]-position)
    fuel_burn[position] <- fuel_burn[position] + 
      ((0.5 * distance^2) + (0.5 * distance))
  }
}

min(fuel_burn)

