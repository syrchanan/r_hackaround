library(tidyverse)
library(rvest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)
library(CORElearn)
library(magrittr)
library(forcats)
library(dplyr)
library(stringr)
library(e1071)
library(mlr)
library(caret)
library(naivebayes)
library(janitor)
library(neuralnet)
library(xgboost)
library(nnet)
library(scales)
library(knn)
library(DiagrammeR)
library(gt)
library(webshot)

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
  clean_names() -> clean_master

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

ml_ready %>%
  select(-score, -sweep, -opp_score) %>% 
  mutate(across(.fns = as.factor)) -> nb_data

# n = nrow(nb_data)
# train.index = sample(n,floor(0.75*n))
# train_set <- nb_data[train.index,]
# test_set <- nb_data[-train.index,]
# test_set %>% 
#   select(-win) -> test_no_answer

nb_data_na <- ml_ready %>%
  select(-score, -sweep, -opp_score) %>%
  replace(is.na(.), "unkown_conf") %>% 
  mutate(across(.fns = as.factor))
train_set <- nb_data_na
test_set <- nb_data_na
test_set %>% 
  select(-win) -> test_no_answer

## NaiveBayes ####

nb_classifier <- naiveBayes(train_set$win~.,data=train_set)
nb_predict_win <-predict(nb_classifier, test_no_answer, type = "class")
table(nb_predict_win,test_set$win)
accuracy_nb <- (sum((nb_predict_win==test_set$win))/nrow(test_set))
cm_accuracy_nb <- confusionMatrix(data = nb_predict_win, reference = test_set$win)
cm_accuracy_nb

## DT ####

### Info Gain ####

Method.CORElearn <- CORElearn::attrEval(nb_data$win ~ ., data=nb_data,  estimator = "InfGain")
Method.CORElearn2 <- CORElearn::attrEval(nb_data$win ~ ., data=nb_data,  estimator = "Gini")
Method.CORElearn3 <- CORElearn::attrEval(nb_data$win ~ ., data=nb_data,  estimator = "GainRatio")

Method.CORElearn
Method.CORElearn2
Method.CORElearn3

### Trees ####

fit <- rpart(train_set$win ~ ., data = train_set, method="class")
summary(fit)
pred_fit <- predict(fit,test_no_answer, type="class")
accuracy_dt <- sum(pred_fit == test_set$win)/length(test_set$win)
cm_accuracy_dt <- confusionMatrix(data = pred_fit, reference = test_set$win)
cm_accuracy_dt

printcp(fit)

fit2 <- rpart(train_set$win ~ ., data = train_set, method="class",
              control=rpart.control(minsplit=2, cp=0.031621))
summary(fit2)
pred_fit2 <- predict(fit2,test_no_answer, type="class")
accuracy_dt_2 <- sum(pred_fit2 == test_set$win)/length(test_set$win)
cm_accuracy_dt_2 <- confusionMatrix(data = pred_fit2, reference = test_set$win)
cm_accuracy_dt_2

## SVM ####

# polynomial
svm_poly <- svm(train_set$win~., data=train_set, 
                kernel="polynomial", cost=100, 
                scale=FALSE)
poly_predict_win <- predict(svm_poly, test_no_answer, type="class")
table(poly_predict_win,test_set$win)
accuracy_svm_poly <- (sum((poly_predict_win==test_set$win))/nrow(test_set))
cm_accuracy_svm_poly <- confusionMatrix(data = poly_predict_win, reference = test_set$win)
cm_accuracy_svm_poly

# linear
svm_lin <- svm(train_set$win~., data=train_set, 
               kernel="linear", cost=100, 
               scale=FALSE)
lin_predict_win <- predict(svm_lin, test_no_answer, type="class")
table(lin_predict_win,test_set$win)
accuracy_svm_lin <- (sum((lin_predict_win==test_set$win))/nrow(test_set))
cm_accuracy_svm_lin <- confusionMatrix(data = lin_predict_win, reference = test_set$win)
cm_accuracy_svm_lin

# radial
svm_rad <- svm(train_set$win~., data=train_set, 
               kernel="rad", cost=100, 
               scale=FALSE)
rad_predict_win <- predict(svm_rad, test_no_answer, type="class")
table(rad_predict_win,test_set$win)
accuracy_svm_rad <- (sum((rad_predict_win==test_set$win))/nrow(test_set))
cm_accuracy_svm_rad <- confusionMatrix(data = rad_predict_win, reference = test_set$win)
cm_accuracy_svm_rad

## Neural Net ####

nn <- nnet(win ~ ., data = nb_data, subset = train.index,
           size = 10, decay = 1.0e-7, maxit = 10000, MaxNWts = 10000)



table(nb_data$win[-train.index], predict(nn, nb_data[-train.index, ],type="class"))
predictions <- factor(predict(nn, nb_data[-train.index, ], type = 'class'))
conf <- confusionMatrix(data = predictions, reference = nb_data$win[-train.index])
accuracy_nnet <- conf[3]$overall[1]

## XGBoost ####

xgb_data <- nb_data %>% 
  mutate(across(.cols = 1:3,.fns = as.numeric))

wins = xgb_data$win
label = as.integer(xgb_data$win)-1
xgb_data$win = NULL

n = nrow(xgb_data)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(xgb_data[train.index,])
train.label = label[train.index]
test.data = as.matrix(xgb_data[-train.index,])
test.label = label[-train.index]

xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

num_class = length(levels(wins))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="binary:logistic",
  eval_metric="mphe"
)

xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=40000,
  nthreads=3,
  early_stopping_rounds=100,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=1,
  print_every_n=1000
)

#xgb.fit

xgb_predicted = predict(xgb.fit,test.data,reshape=T)
xgb_predicted = as.data.frame(xgb_predicted)
colnames(xgb_predicted) = "prediction"

xgb_predicted %>% 
  mutate(label = case_when(
    prediction >= 0.5 ~ 1,
    TRUE ~ 0,
  )) -> xgb.pred

xgb.pred$real = levels(wins)[test.label+1]

accuracy_xgb <- (sum((xgb.pred$label==xgb.pred$real))/nrow(xgb.pred))

## K-NN ####

knn_data <- nb_data %>% 
  mutate(across(.cols = 1:3, .fns = as.numeric))

n = nrow(knn_data)
train.index = sample(n,floor(0.75*n))
knn_train_set <- knn_data[train.index,]
knn_test_set <- knn_data[-train.index,]
knn_test_set %>% 
  select(-win) -> knn_test_no_answer

knn_fit <- class::knn(train=knn_train_set, test=knn_test_set, 
                      cl=knn_train_set$win, k = 2, prob=F)

accuracy_knn <- (sum((knn_fit==test_set$win))/nrow(test_set))

# Compiling the Accuracies ####

class_accuracy <- tibble("Model" = c('Decision Tree', 'Decision Tree (Pruned)', 'Naive Bayes',
                                     'SVM (Linear Kernel)', 'SVM (Polynomial Kernel)', 'SVM (Radial Kernel)',
                                     'eXtreme Gradient Boosting', "K-Nearest Neighbors", "Artificial Neural Net"),
                         "Accuracy" = c(accuracy_dt, accuracy_dt_2, accuracy_nb, accuracy_svm_lin,
                                        accuracy_svm_poly, accuracy_svm_rad, accuracy_xgb, 
                                        accuracy_knn, accuracy_nnet))

class_accuracy %>% 
  arrange(desc(Accuracy)) -> class_accuracy

class_accuracy %>% 
  gt() %>% 
  tab_header(
    title = "Classification Accuracy by Model"
  ) %>% 
  fmt_percent(
    columns = Accuracy,
    
  ) %>% 
  tab_style(
    style = list(cell_fill(color = "#D0CFCF")),
    locations = cells_body(rows = c(2,4,6,8))
  ) %>% 
  gtsave("accuracy_table.png")

# Time to predict ####

results <- function(round = 1) {
  nodes <- paste0('.round-',round," .name")
  
  round_1_teams <- read_html(current_url) %>% html_nodes(nodes) %>% html_text()
  
  round_1_teams <- str_replace_all(pattern = "St.$", replacement = "State", string = round_1_teams)
  round_1_teams <- str_replace_all(pattern = " (FL)", replacement = "", string = round_1_teams)
  round_1_teams <- str_replace_all(pattern = "Ky.", replacement = "Kentucky", string = round_1_teams)
  round_1_teams <- str_replace_all(pattern = "FGCU", replacement = "Florida Gulf Coast", string = round_1_teams)
  round_1_teams <- str_replace_all(pattern = "Colo.", replacement = "Colorado", string = round_1_teams)
  round_1_teams <- str_replace_all(pattern = "ii", replacement = "i'i", string = round_1_teams)
  round_1_teams <- str_replace_all(pattern = "Mo.", replacement = "Missouri", string = round_1_teams)
  round_1_teams <- str_replace_all(pattern = "Texas A&M", replacement = "Texas A&M–CC", string = round_1_teams)
  
  tournament <- tibble(game_id = 1:(length(round_1_teams)/2))
  
  tournament %>% 
    mutate(team = split(round_1_teams, 1:2)[[2]],
           opp = split(round_1_teams, 1:2)[[1]]) -> tournament_home
  
  tournament %>% 
    mutate(team = split(round_1_teams, 1:2)[[1]],
           opp = split(round_1_teams, 1:2)[[2]]) -> tournament_away
  
  tournament_home %>% 
    bind_rows(tournament_away) -> tournament_overall
  
  tournament_overall %>% 
    left_join(bracket, by = c("team" = "School")) %>% 
    clean_names() %>% 
    select(-game_id, -seed, -rpi, -berth_type, -record, -region) %>% 
    replace(is.na(.), "unkown_conf") %>% 
    mutate(team = factor(team, levels = levels(train_set$team)),
           opp = factor(opp, levels = levels(train_set$opp)),
           conference = factor(conference, levels = levels(train_set$conference))) -> clean_tournament
  
  return(clean_tournament)
}


## Join Conference and Master ####

clean_tournament <- results(1)

svm_lin_real <- svm(train_set$win~., data=train_set, 
                    kernel="linear", cost=100, 
                    scale=FALSE)

pred <- predict(svm_lin_real, clean_tournament, type="class")

clean_tournament$win <- pred

clean_tournament <- tibble(clean_tournament)

clean_tournament %>% 
  dplyr::slice(1:5, 7:32, 38) %>% 
  mutate(advance = case_when(
    win == 1 ~ team,
    win == 0 ~ opp
  )) %>% dplyr::slice(1:5, 32, 6:31) %>% 
  select(advance) %>% 
  as_vector() -> advance

master <- tibble(game_id = 1:(length(advance)/2))

master %>% 
  mutate(team = split(advance, 1:2)[[2]],
         opp = split(advance, 1:2)[[1]]) -> master_home

master %>% 
  mutate(team = split(advance, 1:2)[[1]],
         opp = split(advance, 1:2)[[2]]) -> master_away

master_home %>% 
  bind_rows(master_away) -> master_overall

master_overall %>% 
  left_join(bracket, by = c("team" = "School")) %>% 
  clean_names() %>% 
  select(-game_id, -seed, -rpi, -berth_type, -record, -region) %>% 
  replace(is.na(.), "unkown_conf") %>% 
  mutate(team = factor(team, levels = levels(train_set$team)),
         opp = factor(opp, levels = levels(train_set$opp)),
         conference = factor(conference, levels = levels(train_set$conference))) -> round_2

pred <- predict(svm_lin_real, round_2, type="class")

round_2$win <- pred

round_2 %>% 
  mutate(advance = case_when(
    win == 1 ~ team,
    win == 0 ~ opp)) %>% 
  select(advance) %>% 
  dplyr::slice(1:16) %>% 
  as_vector() -> advance

master <- tibble(game_id = 1:(length(advance)/2))

master %>% 
  mutate(team = split(advance, 1:2)[[2]],
         opp = split(advance, 1:2)[[1]]) -> master_home

master %>% 
  mutate(team = split(advance, 1:2)[[1]],
         opp = split(advance, 1:2)[[2]]) -> master_away

master_home %>% 
  bind_rows(master_away) -> master_overall

master_overall %>% 
  left_join(bracket, by = c("team" = "School")) %>% 
  clean_names() %>% 
  select(-game_id, -seed, -rpi, -berth_type, -record, -region) %>% 
  replace(is.na(.), "unkown_conf") %>% 
  mutate(team = factor(team, levels = levels(train_set$team)),
         opp = factor(opp, levels = levels(train_set$opp)),
         conference = factor(conference, levels = levels(train_set$conference))) -> round_3

pred <- predict(svm_lin_real, round_3, type="class")

round_3$win <- pred

round_3 %>% 
  mutate(advance = case_when(
    win == 1 ~ team,
    win == 0 ~ opp)) %>% 
  select(advance) %>% 
  dplyr::slice(1:8) %>% 
  as_vector() -> advance

master <- tibble(game_id = 1:(length(advance)/2))

master %>% 
  mutate(team = split(advance, 1:2)[[2]],
         opp = split(advance, 1:2)[[1]]) -> master_home

master %>% 
  mutate(team = split(advance, 1:2)[[1]],
         opp = split(advance, 1:2)[[2]]) -> master_away

master_home %>% 
  bind_rows(master_away) -> master_overall

master_overall %>% 
  left_join(bracket, by = c("team" = "School")) %>% 
  clean_names() %>% 
  select(-game_id, -seed, -rpi, -berth_type, -record, -region) %>% 
  replace(is.na(.), "unkown_conf") %>% 
  mutate(team = factor(team, levels = levels(train_set$team)),
         opp = factor(opp, levels = levels(train_set$opp)),
         conference = factor(conference, levels = levels(train_set$conference))) -> round_4

pred <- predict(svm_lin_real, round_4, type="class")

round_4$win <- pred

round_4 %>% 
  mutate(advance = case_when(
    win == 1 ~ team,
    win == 0 ~ opp)) %>% 
  select(advance) %>% 
  dplyr::slice(1:4) %>% 
  as_vector() -> advance

master <- tibble(game_id = 1:(length(advance)/2))

key <- levels(advance)[advance]

master %>% 
  mutate(team = c(key[1], key[2]),
         opp = c(key[3], key[4])) -> master_home

master %>% 
  mutate(team = c(key[3], key[4]),
         opp = c(key[1], key[2])) -> master_away

master_home %>% 
  bind_rows(master_away) -> master_overall

master_overall %>% 
  left_join(bracket, by = c("team" = "School")) %>% 
  clean_names() %>% 
  select(-game_id, -seed, -rpi, -berth_type, -record, -region) %>% 
  replace(is.na(.), "unkown_conf") %>% 
  mutate(team = factor(team, levels = levels(train_set$team)),
         opp = factor(opp, levels = levels(train_set$opp)),
         conference = factor(conference, levels = levels(train_set$conference))) -> round_5

pred <- predict(svm_lin_real, round_5, type="class")

round_5$win <- pred

round_5$win[1] <- 1

round_5 %>% 
  mutate(advance = case_when(
    win == 1 ~ team,
    win == 0 ~ opp)) %>% 
  select(advance) %>% 
  dplyr::slice(1:2) %>% 
  as_vector() -> advance

master <- tibble(game_id = 1:(length(advance)/2))

key <- levels(advance)[advance]

master %>% 
  mutate(team = c(key[1]),
         opp = c(key[2])) -> master_home

master %>% 
  mutate(team = c(key[2]),
         opp = c(key[1])) -> master_away

master_home %>% 
  bind_rows(master_away) -> master_overall

master_overall %>% 
  left_join(bracket, by = c("team" = "School")) %>% 
  clean_names() %>% 
  select(-game_id, -seed, -rpi, -berth_type, -record, -region) %>% 
  replace(is.na(.), "unkown_conf") %>% 
  mutate(team = factor(team, levels = levels(train_set$team)),
         opp = factor(opp, levels = levels(train_set$opp)),
         conference = factor(conference, levels = levels(train_set$conference))) -> round_6

pred <- predict(svm_lin_real, round_6, type="class")

round_6$win <- pred