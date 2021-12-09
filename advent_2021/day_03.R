

library(tidyverse)

# Read in Data ####

bits <- read_delim("day_03.txt", delim = "\r\n", 
                         escape_backslash = TRUE, col_names = c("bits"))

bits %>% 
  separate(bits, c("first", "second", "third", "fourth", "fifth", 
                   "sixth", "seventh", "eighth",
                   "ninth", "tenth", "eleventh", "twelfth"), 
           sep = c(1,2,3,4,5,6,7,8,9,10,11)) -> bits_sep

# Part 1 ####

gamma <- c()
epsilon <- c()

for (i in 1:12) {
  one_count <- 0
  zero_count <- 0
  
  for (j in 1:nrow(bits_sep)) {
    if (bits_sep[j,i] == 1) {
      one_count <- one_count + 1
    } else {
      zero_count <- zero_count + 1
    }
  }
  
  if (one_count > zero_count) {
    gamma <- append(gamma, 1)
    epsilon <- append(epsilon, 0)
  } else {
    gamma <- append(gamma, 0)
    epsilon <- append(epsilon, 1)
  }
}

gamma_binary <- strtoi(as.character(paste(gamma, collapse = "")), base = 2)
epsilon_binary <- strtoi(as.character(paste(epsilon, collapse = "")), base = 2)

gamma_binary * epsilon_binary

# Part 2 ####

oxygen_func <- function () {
  while (TRUE) {
    for (i in 1:12) {
      one_count <- 0
      zero_count <- 0
      for (j in 1:nrow(bits_sep)) {
        if (bits_sep[j, i] == 1) {
          one_count <- one_count + 1
        } else {
          zero_count <- zero_count + 1
        }
      }
      if (one_count >= zero_count) {
        bits_sep %>%
          filter(bits_sep[,i] == 1) -> bits_sep
      } else {
        bits_sep %>%
          filter(bits_sep[,i] == 0) -> bits_sep
      }
      if (nrow(bits_sep) == 1) {
        return(bits_sep)
      }
    }
  }
  return(bits_sep)
}

co2_func <- function () {
  while (TRUE) {
    for (i in 1:12) {
      one_count <- 0
      zero_count <- 0
      for (j in 1:nrow(bits_sep)) {
        if (bits_sep[j, i] == 1) {
          one_count <- one_count + 1
        } else {
          zero_count <- zero_count + 1
        }
      }
      if (one_count >= zero_count) {
        bits_sep %>%
          filter(bits_sep[,i] == 0) -> bits_sep
      } else {
        bits_sep %>%
          filter(bits_sep[,i] == 1) -> bits_sep
      }
      if (nrow(bits_sep) == 1) {
        return(bits_sep)
      }
    }
  }
  return(bits_sep)
}

oxygen_generator <- oxygen_func()
co2_scrubber <- co2_func()

oxygen <- strtoi(as.character(paste(as_vector(oxygen_generator[1,]), 
                                    collapse = "")), base = 2)
co2 <- strtoi(as.character(paste(as_vector(co2_scrubber[1,]), 
                                 collapse = "")), base = 2)

oxygen * co2


