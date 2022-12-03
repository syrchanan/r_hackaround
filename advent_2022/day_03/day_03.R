if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

input <- read_lines("./day_03/input.txt")

# part 1 ####

lengths <- map_dbl(input, ~ str_length(.x)/2)

comp1 <- map2_chr(input, lengths, ~ str_sub(.x, 1, .y))

comp2 <- map2_chr(input, lengths, ~ str_sub(.x, .y+1, -1))

matches <- map2_chr(comp1, comp2, ~ intersect(
  unlist(strsplit(.x, split = "")),
  unlist(strsplit(.y, split = ""))
))

priorities <- c(1:52)
names(priorities) <- c(letters, LETTERS)

matches %>% 
  tibble() %>% 
  mutate(scores = priorities[matches]) %>% 
  pull(2) %>% 
  sum()

# Part 2 ####

input %>% 
  tibble() %>% 
  rename("items" = 1) %>% 
  mutate(group = (row_number()+2) %/% 3,
         elf_group = paste0("elf", rep(1:3, length(group)/3))) %>%
  pivot_wider(id_cols = group,
              names_from = elf_group,
              values_from = items) -> elf_wide

common_list <- c()

for (i in 1:nrow(elf_wide)) {
  
  list1 <- unlist(strsplit(elf_wide$elf1[i], split = ""))
  list2 <- unlist(strsplit(elf_wide$elf2[i], split = ""))
  list3 <- unlist(strsplit(elf_wide$elf3[i], split = ""))
  
  common_letter <- Reduce(intersect, list(list1, list2, list3))
  
  common_list <- append(common_list, common_letter)
  
}

sum(priorities[common_list])
