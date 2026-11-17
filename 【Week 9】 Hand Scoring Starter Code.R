# Scoring function
score_hand <- function(hand) 
{
# 1.Straight
 if ( all(diff(sort(hand$rank)) == 1) )   #sort()--little to great, diff()--The difference between adjacent nums, all()--All conditions are met
  {print("Hand is a straight")} 
 else
  {print("Hand is not a straight")}

# 2.Flush
 if ( all(hand$suit == hand$suit[1]) ) 
  {print("Hand is a flush")} 
 else 
  {print("Hand is not a flush")}
  
# 3. Full House
 rank <- sort(hand$rank)
 if ( rank[1]==rank[2] && rank[4]==rank[5] && (rank[3]==rank[1] || rank[3]==rank[4]) )
  {print("Hand is a full house")} 
 else 
  {print("Hand is not a full house")}

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

