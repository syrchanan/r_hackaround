
library(tidyverse)

fish <- as.numeric(str_split(read_lines("day_06.txt"), ",")[[1]])

# Part 1 ####

days <- 80

for (i in 1:days) { #too cost-heavy to compute, will have to
  fish <- fish - 1  #look for other solution that is cleaner
  for (j in 1:length(fish)) {
    if (fish[j] < 0) {
      fish[j] <- 6
      fish <- append(fish, 8)
    }
  }
}

# Part 2 ####

# perhaps matrix math is a more elegant solution?

fish_2 <- as.numeric(str_split(read_lines("day_06.txt"), ",")[[1]])

init <- c(0, table(fish_2), 0, 0, 0)

fish_matrix <- matrix(0, nrow = 9, ncol = 9)
fish_matrix[cbind(2:9, 1:8)] <- 1
fish_matrix[1, c(7,9)] <- 1

reduce(1:256, ~ . %*% fish_matrix, .init = init) %>% sum()

