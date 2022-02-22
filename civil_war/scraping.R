library(tidyverse)
library(jsonlite)
library(httr)

# Soldiers Database ####

url1 <- "https://www.nps.gov/solr-cw/?fl=Image_URL,Title,Side,State,Alternate_Name,Battle_Unit,Battle_Unit_Code,Battle_Unit_Function,PageURL&defType=edismax&fq=Type:%22Soldier%22&fq=Category:%22Civil%20War%22&json.wrf=jQuery112408886962018818634_1644853156885&rows="

url2 <- "00&wt=json&q=*&start="

url3 <- "&facet=true&facet.mincount=1&facet.limit=-1&facet.sort=count&facet.method=enum&facet.field=Side&facet.field=State&facet.field=Battle_Unit_Function&hl=true&hl.fl=text%2C+title&hl.simple.pre=%3Cem%3E&hl.simple.post=%3C%2Fem%3E&hl.snippets=3&_=1644853156887"

#get in 10k iter worked with fromJSON(), just failed to parse because of structure
#grab data with httr::GET() to clean out jquery headers, strip lines, then pass as JSON

add_soldiers <- function(rows, start, master) {
  
  r <- GET(paste0(url1,rows,url2,start,url3))
  
  test_file <- content(r, as = 'text', encoding = 'latin1')
  
  raw_text <- read_lines(test_file)
  
  raw_text[1] <- "{"
  raw_text[length(raw_text)] <- gsub("[()]", "", raw_text[length(raw_text)])
  
  raw_json <- fromJSON(raw_text)
  
  soldiers <- tibble(raw_json[2]$response$docs)

  master <- bind_rows(master, soldiers)
  
  return(master)
}

rows <- 200

start <- 1

master <- tibble()

master <- add_soldiers(1000, 1, master)
master <- add_soldiers(200, 100001, master)

master %>% 
  distinct() %>% 
  nrow()