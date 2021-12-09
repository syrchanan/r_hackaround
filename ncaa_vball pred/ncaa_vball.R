library(tidyverse)
library(rvest)
library(janitor)

# Scraping ####
url <- "https://www.ncaa.com/scoreboard/volleyball-women/d1/2021/08/20/all-conf"
current_url <- "https://www.ncaa.com/brackets/volleyball-women/d1/2021"
conference_data <- "https://en.wikipedia.org/wiki/2021_NCAA_Division_I_Women%27s_Volleyball_Tournament#Qualifying_teams"

## Conf Lookup ####
conferences <- read_html(conference_data) %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[1]/td[1]/table') %>% html_table(fill = TRUE)
louisville_reg <- conferences[[1]]

conferences <- read_html(conference_data) %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[1]/td[2]/table') %>% html_table(fill = TRUE)
madison_reg <- conferences[[1]]

conferences <- read_html(conference_data) %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td[1]/table') %>% html_table(fill = TRUE)
pittsburgh_reg <- conferences[[1]]

conferences <- read_html(conference_data) %>% html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]/tbody/tr[2]/td[2]/table') %>% html_table(fill = TRUE)
austin_reg <- conferences[[1]]

louisville_reg$region <- "louisville"
madison_reg$region <- "madison"
pittsburgh_reg$region <- "pittsburgh"
austin_reg$region <- "austin"

louisville_reg %>% 
  bind_rows(madison_reg, pittsburgh_reg, austin_reg) -> bracket

## Current Teams in tournament ####
current <- read_html(current_url) %>% html_nodes('.round-1 .name') %>% html_text()

## Teams and Scores Iteration ####
get_results <- function() {
  teams_list <- c()
  
  scores_list <- c()
  
  options <- c("08", "09", "10", "11")
  
  count <- 0
  
  for (i in 1:4) {
    month <- options[i]
    if (month == "09" | month == "11") {
      for (day in 1:30) {
        try({
          url <- paste0("https://www.ncaa.com/scoreboard/volleyball-women/d1/2021/",month,"/",day,"/all-conf")
          webpage <- read_html(url)
          
          teams_html <- html_nodes(webpage,'.gamePod-game-team-name') %>% html_text()
          teams_list <- append(teams_list, teams_html)
          
          scores_html <- html_nodes(webpage, '.gamePod-game-team-score') %>% html_text()
          scores_list <- append(scores_list, scores_html)
          
          count <- count + 1
          print(count)
          
          Sys.sleep(0.1)
        }, silent = TRUE)
      }
    } else if (month == "08" | month == "10") {
      for (day in 1:31) {
        try({
          url <- paste0("https://www.ncaa.com/scoreboard/volleyball-women/d1/2021/",month,"/",day,"/all-conf")
          webpage <- read_html(url)
          
          teams_html <- html_nodes(webpage,'.gamePod-game-team-name') %>% html_text()
          teams_list <- append(teams_list, teams_html)
          
          scores_html <- html_nodes(webpage, '.gamePod-game-team-score') %>% html_text()
          scores_list <- append(scores_list, scores_html)
          
          count <- count + 1
          print(count)
          
          Sys.sleep(0.1)
        }, silent = TRUE)
      } 
    }
  }
  
  return(list("teams" = teams_list, "scores" = scores_list))
}

temp <- get_results()

teams_list <- temp$teams
scores_list <- temp$scores

table(teams_list)

teams_list <- str_replace_all(pattern = "St.$", replacement = "State", string = teams_list)
teams_list <- str_replace_all(pattern = " (FL)", replacement = "", string = teams_list)
teams_list <- str_replace_all(pattern = "Ky.", replacement = "Kentucky", string = teams_list)
teams_list <- str_replace_all(pattern = "FGCU", replacement = "Florida Gulf Coast", string = teams_list)
teams_list <- str_replace_all(pattern = "Colo.", replacement = "Colorado", string = teams_list)
teams_list <- str_replace_all(pattern = "ii", replacement = "i'i", string = teams_list)
teams_list <- str_replace_all(pattern = "Mo.", replacement = "Missouri", string = teams_list)
teams_list <- str_replace_all(pattern = "Texas A&M", replacement = "Texas A&M–CC", string = teams_list)

bracket$School[!bracket$School %in% teams_list]

table(teams_list)

# Building Frames ####

master <- tibble(game_id = 1:(length(teams_list)/2))

master %>% 
  mutate(team = split(teams_list, 1:2)[[2]],
         opp = split(teams_list, 1:2)[[1]],
         score = split(scores_list, 1:2)[[2]],
         opp_score = split(scores_list, 1:2)[[1]]) -> master_home

master %>% 
  mutate(team = split(teams_list, 1:2)[[1]],
         opp = split(teams_list, 1:2)[[2]],
         score = split(scores_list, 1:2)[[1]],
         opp_score = split(scores_list, 1:2)[[2]]) -> master_away

master_home %>% 
  bind_rows(master_away) -> master_overall


## Join Conference and Master ####

master_overall %>% 
  left_join(bracket, by = c("team" = "School")) %>% 
  clean_names() %>%
  drop_na(record) -> clean_master

# Add and Edit ####

clean_master %>% 
  filter(!score == "") %>% 
  select(-game_id, -seed, -rpi, -berth_type, -record, -region) %>%
  mutate(sweep = case_when(
    score == 3 & opp_score == 0 ~ 1,
    TRUE ~ 0
  ),
  win = case_when(
    score == 3 ~ 1,
    TRUE ~ 0
  )) -> ml_ready

# Set to factor and hope for best ####
library(e1071)
library(caret)

## NaiveBayes ####

ml_ready %>%
  select(-score, -sweep, -opp_score) %>% 
  mutate(across(.fns = as.factor)) -> nb_data

#NEED TEST AND TRAIN SET
n = nrow(nb_data)
train.index = sample(n,floor(0.75*n))
train_set <- nb_data[train.index,]
test_set <- nb_data[-train.index,]
test_set %>% 
  select(-win) -> test_no_answer


nb_classifier <- naiveBayes(train_set$win~.,data=train_set)
nb_predict_charges <-predict(nb_classifier, test_no_answer, type = "class")
table(nb_predict_charges,test_set$win)
accuracy_nb <- (sum((nb_predict_charges==test_set$win))/nrow(test_set))
cm_accuracy_nb <- confusionMatrix(data = nb_predict_charges, reference = test_set$win)
cm_accuracy_nb

# 652 final

# task 0: 

# 1: shooting efficiency has changed over time, from predominantly inside the 3 pt arc to outside, especially due to steph curry and his revolutionization of the game
# 2: inside the restricted area has the highest shooting percentage, and one of the highest efficiencies. I believe it was ~60% shooting accuracy in the restricted area, which equates to about 1.2 points per shot from there
# 3: Players have started to change their shooting habits - but not everyone successfully. some players have made their mark by being more efficient than the average in a certain shot - the midrange jumper, for example. However, since the 3 pointer has become so efficient, most players are starting to migrate towards shooting them. This will occur until a time where teams become better at denying the 3pt shot, where there will be another migration to back inside the arc.