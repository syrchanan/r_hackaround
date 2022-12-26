if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

areas <- read_lines("./day_04/input.txt")

# Part 1 ####

tibble(areas) %>% 
  separate(areas, c("elf1", "elf2"), sep = ",") %>% 
  mutate(across(everything(), .fns = str_replace_all, "-", ":")) %>% 
  separate(elf1, c("elf1_start", "elf1_finish")) %>% 
  separate(elf2, c("elf2_start", "elf2_finish")) %>% 
  mutate(across(.fns = as.numeric)) %>% 
  mutate(overlap = case_when(
    elf1_start >= elf2_start & elf1_finish <= elf2_finish ~ T,
    elf2_start >= elf1_start & elf2_finish <= elf1_finish ~ T,
    T ~ F
  )) %>% 
  pull(-1) %>% 
  sum()

# Part 2 ####

tibble(areas) %>% 
  separate(areas, c("elf1", "elf2"), sep = ",") %>% 
  mutate(across(everything(), .fns = str_replace_all, "-", ":")) %>% 
  mutate(elf1_areas = map(elf1, 
                          ~ eval(parse(text=.x))),
         elf2_areas = map(elf2,
                          ~ eval(parse(text=.x)))) %>% 
  mutate(overlap = map2(elf1_areas, elf2_areas,
                        ~ intersect(.x, .y)),
         over_len = map(overlap,
                           ~ length(.x))) %>% 
  filter(over_len != 0) %>% 
  nrow()
