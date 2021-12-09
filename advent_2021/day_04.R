
# Read Data and Prep ####

drawing <- scan("day_04.txt", sep = ',', nlines = 1, quiet = TRUE)
boards <- read.table("day_04.txt", skip = 1)

# Part 1 ####

scoring <- function(play, draw) {
  called <- is.na(play)
  if (all(c(rowMeans(called), colMeans(called)) != 1))
    return(0)
  sum(play, na.rm = TRUE) * draw
}

run <- function(drawing, boards) {
  size <- ncol(boards)
  num_cards <- nrow(boards) / size
  board_ids <- rep(1:num_cards, each = size)
  
  for (d in drawing) {
    boards[boards == d] <- NA
    total <- sapply(split(boards, board_ids), scoring, draw = d)
    if (any(total > 0))
      return(total[total > 0])
  }
  
  stop('none')
}

input <- list(drawing, boards)
do.call(run, input)

# Part 2 ####

size <- ncol(boards)

for (d in drawing) {
  num_cards <- nrow(boards) / size
  board_ids <- rep(1:num_cards, each = size)
  boards[boards == d] <- NA
  total <- sapply(split(boards, board_ids), scoring, draw = d)
  if (any(total > 0)) {
    if (num_cards == 1)
      return(total[total > 0])
    boards <- boards[board_ids %in% which(total == 0), ]
  }
}

total
