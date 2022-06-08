if (!require("pacman")) install.packages("pacman")
p_load("tidyverse", "igraph", "rvest", "janitor", "rtweet")

# url <- "https://pressgallery.house.gov/member-data/members-official-twitter-handles"
# 
# reps <- read_html(url) %>% 
#   html_table()
# 
# write_csv(reps[[1]], file = "reps.csv")

reps <- read_csv("reps.csv", skip = 1) %>% 
  clean_names() %>% 
  separate(st_dis, c("st", "dis"), 2)

