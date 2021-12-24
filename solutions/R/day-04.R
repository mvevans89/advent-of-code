#day 4
library(stringr)
library(dplyr)

called_numbers <- as.numeric(str_split(as.character(read.table("../../data/day04.txt", nrows = 1)), pattern = ",")[[1]])
bingo_raw <- read.table("../../data/day04.txt", skip = 2)
#create list of bingo cards
bingo_cards <- list()
bingo_start <- seq(1,nrow(bingo_raw)-4, by = 5)
for(i in 1:length(bingo_start)){
  bingo_cards[[i]] <- as.matrix(bingo_raw[bingo_start[i]:(bingo_start[i]+4),1:5])
}



#function to find number
bingo_score <- lapply(1:100, matrix, data= 0, nrow=5, ncol=5)
round_count <- 0 
game_over <- 0
for (j in called_numbers){
  round_count = round_count + 1
  #find places with those numbers
  for(card in 1:length(bingo_cards)){
    this.card <- bingo_cards[[card]]
    bingo_score[[card]][this.card == j] <- 1
    #check for bingo
    row.bingo = rowSums(bingo_score[[card]])==5
    col.bingo = colSums(bingo_score[[card]])==5
    all.bingo = c(row.bingo, col.bingo)
    if(sum(all.bingo)>0){
      print(paste("winning number:", j))
      print(paste("rounds needed:", round_count))
      print(paste("winning card:", card))
      win_card <- bingo_cards[[card]]
      win_score <- bingo_score[[card]]
      last_num <- j
      game_over <- 1
    }
  }
  if(game_over == T){
    break
  }
}

#get sum of unmarked numbers
score <- sum(win_card[!(win_score)])
#then multiply by the last number called
score
last_num
score*last_num

##Part II####

#create counter for knowing which round a board is completed at
round_win <- rep(NA, length(bingo_cards))
#now rerun the above and update this counter, with no break
#function to find number
bingo_score <- lapply(1:100, matrix, data= 0, nrow=5, ncol=5)
round_count <- 0 
for (j in called_numbers){
  round_count = round_count + 1
  #find places with those numbers
  for(card in 1:length(bingo_cards)){
    this.card <- bingo_cards[[card]]
    bingo_score[[card]][this.card == j] <- 1
    #check for bingo
    row.bingo = rowSums(bingo_score[[card]])==5
    col.bingo = colSums(bingo_score[[card]])==5
    all.bingo = c(row.bingo, col.bingo)
    if(sum(all.bingo)>0 & is.na(round_win[card])){
      round_win[card] <- round_count
      
    }
  }
  if(sum(is.na(round_win))==0){
    break
  }
}

#now find which one wins last
max(round_win)
last_index <- which(round_win == max(round_win))
last_card <- bingo_cards[[last_index]]
last_card_score <- bingo_score[[last_index]]

sum(last_card[!(last_card_score)]) * called_numbers[round_count]
