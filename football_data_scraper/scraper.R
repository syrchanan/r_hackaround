if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "polite", "rvest", "jsonlite")
set.seed(8675309)

url <- "https://sports.yahoo.com/nfl/scoreboard/?confId=&schedState=2&dateRange=18"

session <- bow(url)

scrape(session) %>% 
  html_elements(css = ".Mb\\(3px\\)") %>% 
  html_children() %>% 
  html_text() -> text_output

text_output <- map_chr(text_output, ~str_remove_all(.x, "\\s+"))

text_output <- map_chr(text_output, ~str_replace_all(.x,
                                      "([a-z])([A-Z0-9])",
                                      "\\1 \\2"))

tibble(all = text_output) %>% 
  mutate(score = str_extract(all, "[0-9]+$"),
         team = str_remove(all, "\\s+[0-9]+$")) %>% 
  select(team, score) %>% 
  mutate(home_team = case_when(
    row_number() %% 2 == 0 ~ lag(team),
    T ~ lead(team)
  )) %>% 
  mutate(home_score = case_when(
    row_number() %% 2 == 0 ~ lag(score),
    T ~ lead(score)
  )) %>% 
  filter(row_number() %% 2 == 1) %>% 
  rename("away_team" = team, "away_score" = score) %>% 
  mutate(across(contains("score"), as.numeric)) %>% 
  mutate(winner = case_when(
    away_score > home_score ~ away_team,
    home_score > away_score ~ home_team,
    T ~ "other"
  )) %>% 
  toJSON()
