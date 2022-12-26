if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

# Preprocessing ####

input <- read_lines("./day_07/input.txt")

tibble(input) %>% 
  mutate(
    
    path = input %>% 
      str_extract("(?<=^\\$ cd ).*") %>% #lookahead & lookbehind regex
      str_c("/") %>% 
      replace_na("") %>%
      accumulate( #.y is current item, .x is previous row's result
        ~ if (.y == "../") {
          str_remove(.x, "(?<=/)[a-z]+/$")
        } else {
          str_c(.x, .y)
        }
      ) %>%
      str_remove_all("^/|/$"),
    
    directory = path %>% 
      str_split("/") %>% 
      map(~accumulate(.x, str_c, sep = "/"))
  
  ) %>% 
  
  
  filter(!str_detect(input, "^\\$|dir ")) %>% 
  separate(input, c("size", "file"), " ") %>% 
  mutate(size = as.numeric(size)) %>% 
  
  
  unnest_wider(directory, names_sep = "_") %>% 
  mutate(directory_1 = "/") %>% 
  pivot_longer(cols = matches("directory_\\d+"),
               names_to = NULL,
               values_to = "dir",
               values_drop_na = T) -> file_system
  
# Part 1 ####

puzzle_threshold <- 100000

file_system %>% 
  group_by(dir) %>% 
  summarise(total_size = sum(size)) %>% 
  ungroup() %>% 
  filter(total_size <= puzzle_threshold) %>% 
  pull(total_size) %>% 
  sum()

# Part 2 ####

total_space <- 70000000
needed_space <- 30000000

file_system %>% 
  group_by(dir) %>% 
  summarise(total_size = sum(size)) %>% 
  ungroup() %>% 
  arrange(desc(total_size)) -> grouped_files

used_space <- grouped_files$total_size[1]

space_to_gain <- needed_space - (total_space - used_space)

grouped_files %>% 
  filter(total_size >= space_to_gain) %>% 
  arrange(total_size) %>% 
  slice_min(total_size) %>% 
  pull(total_size)

