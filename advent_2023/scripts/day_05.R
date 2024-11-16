if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "janitor")
set.seed(8675309)

source("./functions.R")

# Parsing ----

data <- import_advent(5, 2023, read_lines("./session"))

# data <- list(raw_data = read_lines("./data/day_05_test.txt"))

data$raw_data %>% 
  head(-1) -> clean_data

(clean_data[1] %>% 
  str_split("\\s")) -> seeds

seeds_adj <- seeds[[1]][2:length(seeds[[1]])]

which(clean_data[3:length(clean_data)] == "")

splitter <- function(x, pos) {
  
  split(x, cumsum(seq_along(x) %in% pos))
  
}

splitter(clean_data[2:length(clean_data)], which(clean_data[2:length(clean_data)] == "")) %>% 
  unname() %>% 
  map(~ .x[2:length(.x)]) %>% 
  map(~ tibble(.x) %>%
        row_to_names(1) %>% 
        rename_with(.fn = ~ str_remove(.x, " map:")) %>% 
        clean_names() %>% 
        separate(col = 1, into = c("rng_dest", "rng_src", "rng_len"), remove = F) %>% 
        mutate(across(contains("rng"), as.numeric)) %>% 
        mutate(rng_dest_max = rng_dest + rng_len -1,
               rng_src_max = rng_src + rng_len -1)) -> conversion_tbls

# Part 1 ----

locations <- c()

for (i in seq_along(seeds_adj)) {
  
  seed <- as.numeric(seeds_adj[i])
  
  for (j in seq_along(conversion_tbls)) {
    
    conversion <- conversion_tbls[[j]]
    
    conversion %>% 
      mutate(src_check = seed >= rng_src & seed <= rng_src_max) %>% 
      filter(src_check) -> filtered_tbl
    
    if (nrow(filtered_tbl) == 0) {
      
      if (j == max(seq_along(conversion_tbls))) {
        
        locations <- append(locations, seed)
        
      }
      
      next
      
    } else {
      
      seed <- (seed - filtered_tbl$rng_src) + filtered_tbl$rng_dest
      
      if (j == max(seq_along(conversion_tbls))) {
        
        locations <- append(locations, seed)
        
      }
      
    }
    
  }
  
}
  
min(locations)

# Part 2 ----
# needs to be more efficient

seeds_adj <- as.numeric(seeds_adj)

seeds_extend <- c()

for (i in 1:(length(seeds_adj)/2)) {
  
  seeds_extend <- append(seeds_extend, seq(seeds_adj[2*i-1], by = 1, length.out = seeds_adj[2*i]))
  
}

locations <- c()

for (i in seq_along(seeds_extend)) {
  
  seed <- as.numeric(seeds_extend[i])
  
  for (j in seq_along(conversion_tbls)) {
    
    conversion <- conversion_tbls[[j]]
    
    conversion %>% 
      mutate(src_check = seed >= rng_src & seed <= rng_src_max) %>% 
      filter(src_check) -> filtered_tbl
    
    if (nrow(filtered_tbl) == 0) {
      
      if (j == max(seq_along(conversion_tbls))) {
        
        locations <- append(locations, seed)
        
      }
      
      next
      
    } else {
      
      seed <- (seed - filtered_tbl$rng_src) + filtered_tbl$rng_dest
      
      if (j == max(seq_along(conversion_tbls))) {
        
        locations <- append(locations, seed)
        
      }
      
    }
    
  }
  
}

min(locations)
