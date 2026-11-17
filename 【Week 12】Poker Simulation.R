## Part 1 Invoking Anna's Function
# 1.Card Deck
hearts <- data.frame(rank = 2:14, suit = "heart") 
clubs <- data.frame(rank = 2:14, suit = "club") 
spades <- data.frame(rank = 2:14, suit = "spade")
diamonds <- data.frame(rank = 2:14, suit = "diamond")
deck <- rbind(hearts, clubs, spades, diamonds)

# 2.Draw W Cards --> Input:deck & number of drawing cards ~ Output:drawing cards & remaining cards  
library(dplyr)
draw_w_hand <- function(deck, n) { 
   hand <- deck |>slice_sample(n = n)
   remaining_deck <- deck |> anti_join(hand)
   return(list(hand, remaining_deck))
}

# 3.Draw N Card from Remaining Deck
draw <- function(deck, n) {
   hand <- deck |> slice_sample(n = n)
   return(hand)
   }

# 4.Hand Value --> Input:5 cards dataframe ~ Output:hand
hand_value <- function(hand) {
  
  ranks <- sort(as.numeric(hand$rank))
  counts <- sort(rle(ranks)$lengths, decreasing = TRUE)
  distinct_ranks <- length(unique(hand$rank))
  
  straight <- length(counts) == 5 &&
    (ranks[5] - ranks[1] == 4 ||
       all(ranks == c(2, 3, 4, 5, 14)))
  
  flush <- all(hand$suit == hand$suit[1])
  
  if (flush == TRUE) {
    if (straight == TRUE) {
      if (identical(ranks, c(10, 11, 12, 13, 14))) {
        return("Royal Flush")
      }
      else {
        return("Straight Flush")
      }
    }
    else {
      return("Flush")
    }
  }
  else if (distinct_ranks == 2) {
    if (ranks[1] == ranks[4] || ranks[2] == ranks[5]) {
      return("Four of a Kind")
    }
    else {
      return("Full House")
    }
  }
  else if (straight == TRUE) {
    return("Straight")
  }
  else if (distinct_ranks == 3) {
    count <- table(ranks)
    
    if (max(count) == 3) {
      return("Three of a Kind")
    }
    else {
      return("Two Pair")
    }
  }
  else if (distinct_ranks == 4) {
    return("One Pair")
  }
  else {
    return("High Card")
  }
}

# 5.Hand Selection --> Input:5/6/7 cards dataframe ~ best hand & best hand card
hand_score <- function(hand) { # created function with hand as argument
  if (nrow(hand) == 5) { # if 5-card hand, return the hand with the value of hand
    return(list(hand_value(hand), hand))
  }
  else {
    hand_order <- c("High Card", "One Pair", "Two Pair", "Three of a Kind",
                    "Straight", "Flush", "Full House", "Four of a Kind",
                    "Straight Flush", "Royal Flush") # created way to order the hands 
    combs <- combn(nrow(hand), 5) # all possible combinations
    best_rank <- 0 
    best_value <- NULL
    best_subset <- NULL # initalized variables for the best rank, value, subset
    for (i in 1:ncol(combs)) { # going through all cominations
      current_subset <- hand[combs[, i], ] # found the current subset we are evaluating
      value <- as.character(hand_value(current_subset)) # found the value of the current subset
      rank <- match(value, hand_order) # found the "rank" of the value of the subset
      if (rank > best_rank) { # if this rank if better than previous, it replaces the best rank
        best_rank <- rank
        best_value <- value
        best_subset <- current_subset
      }
    }
    return(list(best_value, best_subset)) # return best rank
  }
}


## Part 2 Simulation -- 6+1 500 times
# 1.Draw Original Hand
result <- draw_w_hand(deck, 6)
original_hand <- result[[1]]
remaining_deck <- result[[2]]

# 2.Hand Counting Machine
types <- c("Royal Flush", "Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight", "Three of a Kind", "Two Pair", "One Pair", "High Card")
results <- data.frame( Hand_Type = types, Count = rep(0,10))

# 3. Simulation Loop
for (i in 1:500)
{
  new_card <- draw(remaining_deck, 1)
  hand_card <- rbind(original_hand, new_card)
  
  score <- hand_score(hand_card)[[1]]
  row <- which(results$Hand_Type == score)
  
  results$Count[row] <- results$Count[row] + 1
  
}
  
# 4. Calculating Probability
results$Probability <- results$Count / 500

# 5. Output
print(original_hand)
print(results, row.names= FALSE)


## Part 3 Simulation -- 5+2 1000 times
# 1.Draw Original Hand
result <- draw_w_hand(deck, 5)
original_hand <- result[[1]]
remaining_deck <- result[[2]]

# 2.Hand Counting Machine
types <- c("Royal Flush", "Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight", "Three of a Kind", "Two Pair", "One Pair", "High Card")
results <- data.frame( Hand_Type = types, Count = rep(0,10))

# 3. Simulation Loop
for (i in 1:1000)
{
  new_card <- draw(remaining_deck, 2)
  hand_card <- rbind(original_hand, new_card)
  
  score <- hand_score(hand_card)[[1]]
  row <- which(results$Hand_Type == score)
  
  results$Count[row] <- results$Count[row] + 1
  
}

# 4. Calculating Probability
results$Probability <- results$Count / 1000

# 5. Output
print(original_hand)
print(results, row.names= FALSE)


## Part 4 Simulation -- 2+5 5000 times
# 1.Draw Original Hand
result <- draw_w_hand(deck, 2)
original_hand <- result[[1]]
remaining_deck <- result[[2]]

# 2.Hand Counting Machine
types <- c("Royal Flush", "Straight Flush", "Four of a Kind", "Full House", "Flush", "Straight", "Three of a Kind", "Two Pair", "One Pair", "High Card")
results <- data.frame( Hand_Type = types, Count = rep(0,10))

# 3. Simulation Loop
for (i in 1:5000)
{
  new_card <- draw(remaining_deck, 5)
  hand_card <- rbind(original_hand, new_card)
  
  score <- hand_score(hand_card)[[1]]
  row <- which(results$Hand_Type == score)
  
  results$Count[row] <- results$Count[row] + 1
  
}

# 4. Calculating Probability
results$Probability <- results$Count / 5000

# 5. Output
print(original_hand)
print(results, row.names= FALSE)