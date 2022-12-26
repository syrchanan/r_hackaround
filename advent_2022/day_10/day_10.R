if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse")
set.seed(8675309)

# part 1 ####

read_lines("./day_10/input.txt") %>% 
  tibble(raw = .) %>% 
  separate(raw, c("command", "amt"), sep = " ") %>% 
  mutate(amt = as.numeric(amt)) %>% 
  replace_na(list(amt = 0)) -> input

vals <- data.frame(cycle = 1, amt = 1)

global_cycle <- 1

for (i in 1:nrow(input)) {
  
  global_cycle <- global_cycle + 1
  
  if (input$command[i] == "noop") {
    new_row <- c(global_cycle, rev(vals$amt)[1])
    vals <- rbind(vals, new_row)
  } 
  
  if (input$command[i] == "addx") {
    
    new_row <- c(global_cycle, rev(vals$amt)[1])
    vals <- rbind(vals, new_row)
    
    global_cycle <- global_cycle + 1
    
    new_row2 <- c(global_cycle, rev(vals$amt)[1] + input$amt[i])
    vals <- rbind(vals, new_row2)
    
  }
  
}

vals %>% 
  tibble() %>% 
  filter(cycle %in% c(20, 60, 100, 140, 180, 220)) %>% 
  mutate(strength = cycle * amt) %>% 
  pull(strength) %>% 
  sum()

# part 2 ####

#amt is register
vals <- data.frame(cycle = 0, amt = 2, pxl = "")

global_cycle <- 0

i = 1

for (i in 1:nrow(input)) {
  
  global_cycle <- global_cycle + 1
  
  if (input$command[i] == "noop") {
    
    if ((global_cycle %% 40) %in% c(as.numeric(rev(vals$amt)[1])-1, as.numeric(rev(vals$amt)[1]), as.numeric(rev(vals$amt)[1])+1)) {
      new_row <- c(global_cycle, as.numeric(rev(vals$amt)[1]), "#")
    } else {
      new_row <- c(global_cycle, as.numeric(rev(vals$amt)[1]), ".")
    }
    vals <- rbind(vals, new_row)
  } 
  
  if (input$command[i] == "addx") {
    
    if ((global_cycle %% 40) %in% c(as.numeric(rev(vals$amt)[1])-1, as.numeric(rev(vals$amt)[1]), as.numeric(rev(vals$amt)[1])+1)) {
      new_row <- c(global_cycle, as.numeric(rev(vals$amt)[1]), "#")
    } else {
      new_row <- c(global_cycle, as.numeric(rev(vals$amt)[1]), ".")
    }

    vals <- rbind(vals, new_row)
    
    global_cycle <- global_cycle + 1
    
    if ((global_cycle %% 40) %in% c(as.numeric(rev(vals$amt)[1])-1, as.numeric(rev(vals$amt)[1]), as.numeric(rev(vals$amt)[1])+1)) {
      new_row <- c(global_cycle, as.numeric(rev(vals$amt)[1]) + input$amt[i], "#")
    } else {
      new_row <- c(global_cycle, as.numeric(rev(vals$amt)[1]) + input$amt[i], ".")
    }
    
    vals <- rbind(vals, new_row)
    
  }
  
}

#remove initial
vals <- vals[2:nrow(vals),]

#fix mod div error
vals$pxl[40] <- "."
vals$pxl[80] <- "."
vals$pxl[120] <- "."
vals$pxl[160] <- "."
vals$pxl[200] <- "."
vals$pxl[240] <- "."

#paste for viewing in environment
pt1 <- str_c(vals$pxl[1:40], collapse = "")
pt2 <- str_c(vals$pxl[41:80], collapse = "")
pt3 <- str_c(vals$pxl[81:120], collapse = "")
pt4 <- str_c(vals$pxl[121:160], collapse = "")
pt5 <- str_c(vals$pxl[161:200], collapse = "")
pt6 <- str_c(vals$pxl[201:240], collapse = "")

# RLEZFLGE
