
library(tidyverse)

input <- read_lines("day_08.txt")
sample <- read_lines("day_08_sample.txt")

# Part 1 ####

digits_sep <- str_split(input, pattern = " ")

count <- 0
for (i in 1:length(digits_sep)) {
  temp <- digits_sep[[i]][12:15]
  count <- count + length(which(nchar(temp)==2 | nchar(temp)==3 | 
                                  nchar(temp)==4 | nchar(temp)==7))
}

# Part 2 ####

digits_sep <- str_split(input, pattern = " ")
overall <- 0

for(i in 1:length(digits_sep)){
  code <- digits_sep[[i]][12:15]
  shapes <- digits_sep[[i]][1:10]
  
  shape1 <- str_split(shapes[which(nchar(shapes)==2)],pattern = "")[[1]] #1
  shape4 <- str_split(shapes[which(nchar(shapes)==4)],pattern = "")[[1]] #4
  shape7 <- str_split(shapes[which(nchar(shapes)==3)],pattern = "")[[1]] #7
  shape8 <- str_split(shapes[which(nchar(shapes)==7)],pattern = "")[[1]] #8
  
  shape069 <- shapes[which(nchar(shapes)==6)] #0,6,9
  shape235 <- shapes[which(nchar(shapes)==5)] #2,3,5
  
  for(j in 1:length(shape069)){
    shape_ <- str_split(shape069[j],pattern = "")[[1]]
    
    a <- shape1[! shape1 %in% shape_] 
    b <- shape1[! shape4 %in% shape_] 
    
    if(length(a)==0 & length(b)==0){
      shape9 <- str_split(shape069[j],pattern = "")[[1]]
    }else if(length(a)!=0 & length(b)!=0){
      shape6 <- str_split(shape069[j],pattern = "")[[1]]
    }else{
      shape0 <- str_split(shape069[j],pattern = "")[[1]]
    }
  }
  
  top_right_char <- shape1[! shape1 %in% shape6] 
  
  for(j in 1:length(shape235)){
    shape_ <- str_split(shape235[j],pattern = "")[[1]]
    
    a <- length(shape1[! shape1 %in% shape_])==0
    b <- top_right_char %in% shape_
    
    if(a){
      shape3 <- str_split(shape235[j],pattern = "")[[1]]
    }else if(b){
      shape2 <- str_split(shape235[j],pattern = "")[[1]]
    }else{
      shape5 <- str_split(shape235[j],pattern = "")[[1]]
    }
  }
  
  shapes_list <- list(shape0,shape1,shape2,shape3,shape4,shape5,shape6,
                      shape7,shape8,shape9)
  code_deciphered <- vector()
  for(k in 1:length(code)){
    code_ <- str_split(code[k],pattern = "")[[1]]
    
    for(p in 1:length(shapes_list)){
      if(setequal(code_,shapes_list[[p]])){
        code_deciphered <- append(code_deciphered,(p-1))
      }
    }
  }
  code_deciphered_val <- as.numeric(paste0(code_deciphered,collapse=""))
  overall <- overall + code_deciphered_val
}

