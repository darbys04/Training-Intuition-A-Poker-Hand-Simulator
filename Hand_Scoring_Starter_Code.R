# Scoring function
score_hand <- function(hand) {
  
  # Check if hand is a straight
  if (?????????) {
    print("Hand is a straight")
  } else {
    print("Hand is not a straight")
  }
  
  # Check if hand is a flush
  if (?????????) {
    print("Hand is a flush")
  } else {
    print("Hand is not a flush")
  }
  
  # Check if hand is a full house
  if (?????????) {
    print("Hand is a full house")
  } else {
    print("Hand is not a full house")
  }
}


############################# Test cases ##################################

# Pair
hand.pair <- data.frame(rank = c(3, 5, 6, 8, 5), suit = c("H", "H", "D", "C", "C"))
print("Testing pair")
score_hand(hand = hand.pair)

# Flush
hand.flush <- data.frame(rank = c(3, 5, 6, 8, 5), suit = c("H", "H", "H", "H", "H"))
print("Testing flush")
score_hand(hand = hand.flush)

# Straight
hand.straight <- data.frame(rank = c(5, 7, 6, 8, 4), suit = c("H", "S", "H", "D", "H"))
print("Testing straight")
score_hand(hand = hand.straight)

# Full House
hand.fullhouse <- data.frame(rank = c(5, 7, 7, 5, 7), suit = c("H", "S", "H", "D", "D"))
print("Testing full house")
score_hand(hand = hand.fullhouse)

