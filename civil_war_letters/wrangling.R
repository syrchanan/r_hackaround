if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "tidytext", "textdata", "hunspell")
set.seed(8675309)

metadata_tbl <- readRDS("./all_letters_tbl.RDS")
letters <- readRDS("./all_text.RDS")

metadata_tbl %>% 
  left_join(letters, join_by(node_path),
            relationship = "many-to-many") %>% 
  rename(text = letter_text) %>% 
  mutate(text = str_replace_all(text, "(\\s+)", " ")) %>% 
  mutate(letter_id = row_number()) -> clean_letters

# Sentiment Analysis ----

clean_letters %>% 
  select(letter_id, text) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, join_by(word)) -> tidy_letters

tidy_letters %>% 
  count(word, sort = T)

tidy_letters %>% 
  left_join(get_sentiments("bing"),
            relationship = "many-to-many") %>% 
  rename(bing = sentiment) %>% 
  left_join(get_sentiments("nrc"),
            relationship = "many-to-many") %>% 
  rename(nrc = sentiment) %>% 
  left_join(get_sentiments("afinn"),
            relationship = "many-to-many") %>% 
  rename(afinn = value) %>% 
  left_join(get_sentiments("loughran"),
            relationship = "many-to-many") %>% 
  rename(loughran = sentiment) -> sentiment_letters

sentiment_letters %>% 
  group_by(letter_id) %>% 
  summarise(net_afinn = sum(afinn, na.rm = T)) %>% 
  left_join(clean_letters, ., join_by(letter_id)) %>% 
  slice(sample(nrow(.), 1)) %>% 
  pull(text) %>% cat()

