library(tidyverse)

url <- "http://data.gdeltproject.org/gdeltv3/iatv/ngrams/FILELIST-CNN.TXT"

R.utils::gunzip("20220520.CNN.1gram.txt.gz")

ngrams <- read_tsv("20220520.CNN.1gram.txt", col_names = c("date", "network", "time", "word", "count"))
