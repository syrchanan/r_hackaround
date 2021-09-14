# Association Rules ####

setwd("C:/Users/cdawg/git_repos/R hackaround")

library(pacman)
p_load("arules", "arulesViz", "tidyverse")

data <- read_csv("all_yan.csv")

data %>% 
  select(-`NY Time`, -SL, -Sum) %>% 
  replace(is.na(.), 0) %>% 
  mutate_all(as.factor) -> clean_data

prop.table(table(clean_data$TP))

dataX <- as(clean_data, 'transactions')

itemFrequency(dataX)
itemFrequencyPlot(dataX)

rules <- apriori(dataX, parameter=list(support=0.0001, confidence=0.3), appearance=list(rhs="TP=1"))

arules::write(rules, "rules.csv", sep = ",")

rules_csv <- tibble(read.csv("rules.csv"))

rules_csv %>% 
  mutate(rules = gsub(" => \\{TP=1\\}", "", rules)) %>% 
  #mutate(rules = gsub("\\{","", rules)) %>% 
  #mutate(rules = gsub("\\}","", rules)) %>%
  filter(count >= 10) %>% 
  arrange(desc(confidence)) -> rules_output

write_csv(rules_output, "rules_output.csv")

# With Optimal Risk Grouping ####

library(tidyverse)

yan_optimal <- read_csv("rules_output Profitability metric.xlsx - Sheet1.csv")

yan_optimal %>% 
  rename_all(tolower) %>% 
  mutate(`optimal risk` = as.numeric(gsub("[$,]", '', `optimal risk`))) %>% 
  mutate(return = as.numeric(gsub("[$,]", '', return))) -> clean_optimal

clean_optimal %>% 
  arrange(desc(return)) %>% 
  mutate(return_rank = rank(-return, ties.method = "min")) %>% 
  arrange(desc(confidence)) %>% 
  mutate(confidence_rank = rank(-confidence, ties.method = "min")) %>% 
  arrange(desc(`count/data`)) %>% 
  mutate(count_rank = rank(-`count/data`, ties.method = "min")) %>% 
  arrange(desc(`optimal risk`)) %>% 
  mutate(risk_rank = rank(-`optimal risk`, ties.method = "min")) %>% 
  mutate(avg_rank = (return_rank + confidence_rank + count_rank + risk_rank)/4) %>% View()

# LM of TP ####

data %>% 
  select(-`NY Time`, -SL, -Sum) %>% 
  replace(is.na(.), 0) -> non_factor

lmout <- lm(TP ~ Time+`Local Correct`+`Global Correct`, non_factor)

summary(lmout)

# Calc return per rule ####

library(tidyverse)

data <- read_csv("rules_output.csv")

return_threshold <- function(data, threshold) {
  
  data %>% 
    rename(tp = count) %>% 
    mutate(sl = (tp/confidence)-tp) %>% 
    mutate(total = tp/confidence) -> clean_data
  
  risk_opt <- tibble(risk = seq(from = 100, to = 3000, by = 100))
  risk_opt %>% 
    mutate(reward = 2.5*risk) %>% 
    mutate(num_failed_month = 10000/risk) -> risk_opt
  
  opt_risk_vec <- list()
  
  for (row in 1:nrow(clean_data)) {
    accuracy <- as.numeric(clean_data[row, 3])
    tp <- as.numeric(clean_data[row, 6])
    sl <- as.numeric(clean_data[row, 7])
    total <- as.numeric(clean_data[row, 8])
    
    temp_risk <- risk_opt
    
    temp_risk %>% 
      mutate(month_fail_prob = round((100*(1-accuracy)^(temp_risk$num_failed_month))),
             day_fail_prob = round(100*(1-accuracy)^(temp_risk$num_failed_month/2))) %>%
      filter(month_fail_prob + day_fail_prob <= threshold) %>% 
      arrange(desc(risk)) -> temp_sorted
    
    opt_risk_vec[[row]] <- temp_sorted$risk[1]
  }
  
  clean_data %>% 
    mutate(opt_risk = as.numeric(opt_risk_vec),
           return = (tp*opt_risk*2.5)+(sl*opt_risk*-1)) %>% 
    arrange(desc(return)) -> return_data
  
  return(return_data)
}

zero_percent <- return_threshold(data, 0) %>% arrange(rules) %>% select(rules, return) %>% rename(zero = return)

return_threshold(data, 5) %>% 
  arrange(rules) %>%
  #select(rules, return) %>% 
  rename(threshold = return) %>% 
  left_join(zero_percent) %>% 
  mutate(pct_incr_from_zero_risk = round(100*(threshold/zero))-100) %>% 
  write_csv(file = "five_percent_comparison.csv")

# Calc freq per pair ####

setwd("C:/Users/cdawg/git_repos/R hackaround")

library(pacman)
p_load("arules", "arulesViz", "tidyverse")

data <- read_csv("all_yan.csv")

data %>% 
  select(-`NY Time`, -SL, -Sum) %>% 
  replace(is.na(.), 0) %>% 
  rename_all(tolower)-> clean_data

clean_data %>% 
  filter(tp != 1) %>% 
  group_by(pair) %>% 
  summarise(sl = n()) -> pair_sl
clean_data %>% 
  filter(tp == 1) %>% 
  group_by(pair) %>% 
  summarise(tp = n()) %>% 
  left_join(pair_sl) %>% 
  mutate(total = tp + sl,
         accuracy = tp/total,
         tp_month = round(tp/(69/21.5)),
         sl_month = round(sl/(69/21.5)),
         total_month = round(total/(69/21.5))) %>% View()

# not sure where to go from here

