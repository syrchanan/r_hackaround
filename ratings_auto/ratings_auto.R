library(tidyverse)
library(rvest)
library(lubridate)
library(gt)
library(janitor)

# Scraping ####

target <- Sys.Date()-2
week_day <- tolower(wday(target, T, F))
abbr_month <- tolower(month(target, T, T))
day_num <- day(target)


url <- paste0("https://www.adweek.com/tvnewser/",week_day,"-",abbr_month,"-",day_num)

ratings <- read_html(url) %>% html_nodes('table') %>% html_table()

ratings_25_54 <- ratings[[1]]
ratings_td <- ratings[[2]]

scores <- read_html(url) %>% html_nodes('.entry-wrap .entry-content.content p') %>% html_text()


# Formatting in DT ####

ratings_25_54 %>%
  clean_names() %>%
  gt() %>%
  tab_header(title = "25-54 Demographic (Live+SD X 1,000)") %>%
  cols_label(x = "", fnc = "FOXN", cnn = "CNN", msnbc = "MSNBC") %>%
  tab_style(
    style = cell_borders(
      sides = c('left', 'right'),
      color = "#BBBBBB",
      weight = px(1),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  tab_style(
    style = cell_fill(
      color = "#D3D3D3"
    ),
    locations = cells_body(
      columns = everything(),
      rows = c(2,4,6,8)
    )
  ) %>%
  gtsave("total_viewers.png")

ratings_td %>%
  clean_names() %>%
  gt() %>%
  tab_header(title = "Total Viewers (Live+SD X 1,000)") %>%
  cols_label(x = "", fnc = "FOXN", cnn = "CNN", msnbc = "MSNBC") %>%
  tab_style(
    style = cell_borders(
      sides = c('left', 'right'),
      color = "#BBBBBB",
      weight = px(1),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  tab_style(
    style = cell_fill(
      color = "#D3D3D3"
    ),
    locations = cells_body(
      columns = everything(),
      rows = c(2,4,6,8)
    )
  ) %>%
  gtsave("total_viewers.png")

# Send Email ####

#install.packages("rJava")
library(rJava)
#install.packages("mailR")
library(mailR)

subject_line <- paste0("Ratings for: ", wday(target, T, T), ", ", month(target, T, T), " ", wday(target), ", ", year(target))
body_scores <- paste0(scores[1],"\n",scores[2],"\n\n",scores[3],"\n",scores[4])

info <- read_lines("emails_passwords.txt")

sender <- info[1]
recipients <- c(info[2])
send.mail(from = sender,
           to = recipients,
           subject = subject_line,
           body = body_scores,
           attach.files = c("25_54.png", "total_viewers.png"),
           smtp = list(host.name = "smtp.gmail.com", port = 465,
                       user.name = info[3],
                       passwd = info[4], ssl = TRUE),
           authenticate = TRUE,
           send = TRUE)

rm(list = ls())
