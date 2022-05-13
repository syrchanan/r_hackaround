#import libs ####
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)

#Ratings History ####
response <- GET("https://aoeiv.net/api/player/ratinghistory?game=aoe4&leaderboard_id=17&steam_id=76561198119476373&count=1000")

rating_history <- fromJSON(rawToChar(response$content))

rating_history %>% 
  mutate(timestamp = as_datetime(timestamp)) %>%
  arrange(desc(timestamp)) %>% 
  slice(1) -> recent_rating

#Match History ####
response <- GET("https://aoeiv.net/api/player/matches?game=aoe4&leaderboard_id=17&steam_id=76561198119476373&count=1000")

match_history <- fromJSON(rawToChar(response$content))
