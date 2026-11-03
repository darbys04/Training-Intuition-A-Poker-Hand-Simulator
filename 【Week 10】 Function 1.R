# 1.1 Define Function
score_hand <- function(hand)
{
  rank <- sort(hand$rank)
# 2.1 Royal Flush
  if ( all(sort(hand$rank) == c(10, 11, 12, 13, 14)) && all(hand$suit == hand$suit[1]) ) 
    return("Royal Flush")

# 2.2 Straight Flush
  else if( all(diff(sort(hand$rank)) == 1) && all(hand$suit == hand$suit[1]) )
    return("Straight Flush")

# 2.3 Four of a Kind
  else if( any(table(hand$rank) == 4) ) #table()--count numbers
    return("Four of a Kind")

# 2.4 Full House
  else if( rank[1]==rank[2] && rank[4]==rank[5] && (rank[3]==rank[1] || rank[3]==rank[4]) )
    return("Full House")

# 2.5 Flush
  else if( all(hand$suit == hand$suit[1]) )
    return("Flush")

# 2.6 Straight
  else if( all(diff(sort(hand$rank)) == 1) )
    return("Straight")

# 2.7 Three of a Kind
  else if( any(table(hand$rank) == 3) )
    return("Three of a Kind")

# 2.8 Two Pair
  else if( all(sort(table(hand$rank)) == c(1,2,2)) )
    return("Two Pair")

# 2.9 One Pair
  else if( all(sort(table(hand$rank)) == c(1,1,1,2)) )
    return("One Pair")

# 2.10 High Card
  else
    return("High Card")
} 