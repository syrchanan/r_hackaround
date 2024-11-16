if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "rvest", "polite", "janitor")
set.seed(8675309)

# Get all the links ----

url <- "https://altchive.org/letters"
iters <- c(0:190)


session <- bow(url, force = T)


all_letters <- tibble()

for (i in iters) {
  
  scrape(session, query = list(page = i)) %>%
    html_table() %>%
    pluck(1) -> raw_data
  
  scrape(session, query = list(page = i)) %>%
    html_elements(css = ".views-field-title") %>%
    html_children() %>%
    html_attrs() %>%
    .[-1] %>%
    unname() %>%
    list_c() %>%
    bind_cols(raw_data, ., .name_repair = "unique_quiet") %>%
    clean_names() %>%
    rename(node_path = "x8") %>%
    tibble() -> full_data_urls
  
  all_letters <- bind_rows(all_letters, full_data_urls)
  
  print(paste(">>> Page", i+1, "of", max(iters)+1, "complete."))
  
  Sys.sleep(1)
  
}

all_letters %>%
  distinct() %>%
  saveRDS("all_letters_tbl.RDS")


all_letters <- readRDS("./all_letters_tbl.RDS")

all_letters$node_path

# Get all the letters ----

url <- "https://altchive.org"
iters <- c(1:length(all_letters$node_path))

all_text <- readRDS("./all_text.RDS")

for (i in (nrow(all_text)+1):(max(iters))) {
  
  # session <- bow(paste0(url, all_letters$node_path[i]), 
  #                force = T,
  #                delay = 1)
  
  # scrape(session) %>%
  read_html(paste0(url, all_letters$node_path[i])) %>% 
    html_elements(css = "p") %>% 
    html_text() %>% 
    .[1] %>% 
    tibble(letter_text = .,
           node_path = all_letters$node_path[i]) -> full_letter_text
  
  all_text <- bind_rows(all_text, full_letter_text)
  
  print(paste(">>> Letter", i, "of", max(iters), "complete."))
  
  if (i %% 100 == 0) {
    
    saveRDS(all_text, "all_text.RDS")
    
    print(paste(">>> NOTE: Cached chunk", i %/% 100, "."))
    
    Sys.sleep(20)
    
  } else {
   
    Sys.sleep(2) 
    
  }
  
}


saveRDS(all_text, "all_text.RDS")

readRDS("./all_text.RDS") %>% View()
