# 1.1 Tie_Breaker Function 
Tie_Breaker <- function(hand_1, hand_2, score)
{
  # 2.1 Royal Flush
  if( score == "Royal Flush" )
    return( "Tie" )
  
  
  # 2.2 Straight Flush
  else if( score == "Straight Flush" )
  {
    rank_1 <- sort(hand_1$rank)
    rank_2 <- sort(hand_2$rank)
    
    high_1 <- max(rank_1)
    high_2 <- max(rank_2)
    
    if( all(rank_1 == c(2, 3, 4, 5, 14)) )
      high_1 <- 5
    if( all(rank_2 == c(2, 3, 4, 5, 14)) )
      high_2 <- 5
    
    if( high_1 > high_2 )
      return( hand_1 )
    else if( high_1 < high_2 )
      return( hand_2 )
    else 
      return( "Tie" )
  }
  
  
  # 2.3 Four of a Kind
  else if( score == "Four of a Kind" )
  {
    rank_1 <- sort(hand_1$rank)
    rank_2 <- sort(hand_2$rank)
    
    four_1 <- rank_1[2]
    four_2 <- rank_2[2]
    
    one_1 <- rank_1[rank_1 != four_1]
    one_2 <- rank_2[rank_2 != four_2]
    
    if( four_1 > four_2 )
      return( hand_1 )
    else if( four_1 < four_2 )
      return( hand_2 )
    else if( one_1 > one_2 )
      return( hand_1 )
    else if( one_1 < one_2 )
      return( hand_2 )
    else
      return( "Tie")
  }
  
  
  # 2.4 Full House
  else if( score == "Full House" )
  {
    rank_1 <- sort(hand_1$rank)
    rank_2 <- sort(hand_2$rank)
    
    three_1 <- rank_1[3]
    three_2 <- rank_2[3]
    
    pair_1 <- unique(rank_1[rank_1 != three_1])
    pair_2 <- unique(rank_2[rank_2 != three_2])
    
    if( three_1 > three_2 )
      return( hand_1 )
    else if( three_1 < three_2 ) 
      return( hand_2 )
    else if( pair_1 > pair_2 )
      return( hand_1 )
    else if( pair_1 < pair_2 )
      return( hand_2 )
    else
      return( "Tie" )
  }
  
  
  # 2.5 Flush
  else if( score == "Flush" )
  {
    rank_1 <- sort(hand_1$rank)
    rank_2 <- sort(hand_2$rank)
    
    for (i in 5:1)
    {
      if( rank_1[i] > rank_2[i] )
        return( hand_1 )
      else if( rank_1[i] < rank_2[i] )
        return( hand_2)
    }
    
    return( "Tie" )
  }
  
  
  # 2.6 Straight
  else if( score == "Straight" )
  {
    rank_1 <- sort(hand_1$rank)
    rank_2 <- sort(hand_2$rank)
    
    high_1 <- max(rank_1)
    high_2 <- max(rank_2)
    
    if( all(rank_1 == c(2, 3, 4, 5, 14)) )
      high_1 <- 5
    if( all(rank_2 == c(2, 3, 4, 5, 14)) )
      high_2 <- 5
    
    if( high_1 > high_2 )
      return( hand_1 )
    else if( high_1 < high_2 )
      return( hand_2 )
    else
      return( "Tie" )
  }
  
  
  # 2.7 Three of a Kind
  else if( score == "Three of a Kind")
  {
    rank_1 <- sort(hand_1$rank)
    rank_2 <- sort(hand_2$rank)
    
    three_1 <- rank_1[3]
    three_2 <- rank_2[3]
    
    one_1 <- rank_1[rank_1 != three_1]
    one_2 <- rank_2[rank_2 != three_2]
    
    if( three_1 > three_2 )
      return( hand_1 )
    else if( three_1 < three_2 )
      return( hand_2)
    
    else if( one_1[2] > one_2[2] )
      return( hand_1 )
    else if( one_1[2] < one_2[2] )
      return( hand_2 )
    
    else if( one_1[1] > one_2[1] )
      return( hand_1 )
    else if( one_1[1] < one_2[1] )
      return( hand_2 )
    else 
      return( "Tie" )
  }
  
  
  # 2.8 Two Pair
  else if( score == "Two Pair")
  {
    rank_1 <- sort(hand_1$rank)
    rank_2 <- sort(hand_2$rank)
    
    high_pair_1 <- rank_1[4]
    high_pair_2 <- rank_2[4]
    
    low_pair_1 <- rank_1[2]
    low_pair_2 <- rank_2[2]
    
    one_1 <- rank_1[rank_1 != high_pair_1 & rank_1 != low_pair_1]
    one_2 <- rank_2[rank_2 != high_pair_2 & rank_2 != low_pair_2]
    
    if( high_pair_1 > high_pair_2) 
      return( hand_1 )
    else if( high_pair_1 < high_pair_2 )
      return( hand_2 )
    
    else if( low_pair_1 > low_pair_2 )
      return( hand_1 )
    else if( low_pair_1 < low_pair_2 )
      return( hand_2 )
    
    else if( one_1 > one_2 )
      return( hand_1 )
    else if( one_1 < one_2 )
      return( hand_2 )
    else
      return( "Tie" )
  }
  
  
  # 2.9 One Pair
  else if( score == "One Pair" )
  {
    rank_1 <- sort( hand_1$rank )
    rank_2 <- sort( hand_2$rank )
    
    for(i in 1:4)
    {
      if( rank_1[i] == rank_1[i+1] )
        pair_1 <- rank_1[i]
      
      if( rank_2[i] == rank_2[i+1] )
        pair_2 <- rank_2[i]
    }
    
    if( pair_1 > pair_2 )
      return( hand_1 )
    else if( pair_1 < pair_2 )
      return( hand_2 )
    
    one_1 <- rank_1[rank_1 != pair_1]
    one_2 <- rank_2[rank_2 != pair_2]
    
    for(i in 3:1)
    {
      if( one_1[i] > one_2[i] )
        return( hand_1 )
      else if( one_1[i] < one_2[i] )
        return( hand_2 )
    }
    
    return( "Tie" )
  }
  
  
  # 2.10 High card
  else if( score == "High Card" )
  {
    rank_1 <- sort(hand_1$rank)
    rank_2 <- sort(hand_2$rank)
    
    for( i in 5:1 )
    {
      if( rank_1[i] > rank_2[i] )
        return( hand_1 )
      else if( rank_1[i] < rank_2[i] )
        return( hand_2 )
    }
    
    return( "Tie" )
  }
  
}


# 2.1 Royal Flush
# Test 1 - Tie
hand_1 <- data.frame(rank = c(10, 11, 12, 13, 14), suit = c("H", "H", "H", "H", "H"))
hand_2 <- data.frame(rank = c(10, 11, 12, 13, 14), suit = c("S", "S", "S", "S", "S"))
print(Tie_Breaker(hand_1, hand_2, "Royal Flush"))

# Test 2 - Tie
hand_1 <- data.frame(rank = c(14, 13, 12, 11, 10), suit = c("D", "D", "D", "D", "D"))
hand_2 <- data.frame(rank = c(10, 12, 14, 11, 13), suit = c("C", "C", "C", "C", "C"))
print(Tie_Breaker(hand_1, hand_2, "Royal Flush"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(11, 13, 10, 14, 12), suit = c("H", "H", "H", "H", "H"))
hand_2 <- data.frame(rank = c(12, 10, 14, 13, 11), suit = c("D", "D", "D", "D", "D"))
print(Tie_Breaker(hand_1, hand_2, "Royal Flush"))


# 2.2 Straight Flush
# Test 1 - Player 1 Wins
hand_1 <- data.frame(rank = c(5, 6, 7, 8, 9), suit = c("H", "H", "H", "H", "H"))
hand_2 <- data.frame(rank = c(4, 5, 6, 7, 8), suit = c("S", "S", "S", "S", "S"))
print(Tie_Breaker(hand_1, hand_2, "Straight Flush"))

# Test 2 - Player 2 Wins
hand_1 <- data.frame(rank = c(14, 2, 3, 4, 5), suit = c("C", "C", "C", "C", "C"))
hand_2 <- data.frame(rank = c(2, 3, 4, 5, 6), suit = c("D", "D", "D", "D", "D"))
print(Tie_Breaker(hand_1, hand_2, "Straight Flush"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(6, 7, 8, 9, 10), suit = c("H", "H", "H", "H", "H"))
hand_2 <- data.frame(rank = c(10, 9, 8, 7, 6), suit = c("D", "D", "D", "D", "D"))
print(Tie_Breaker(hand_1, hand_2, "Straight Flush"))


# 2.3 Four of a Kind
# Test 1 - Player 1 Wins
hand_1 <- data.frame(rank = c(11, 11, 11, 11, 2), suit = c("H", "D", "C", "S", "H"))
hand_2 <- data.frame(rank = c(9, 9, 9, 9, 14), suit = c("H", "D", "C", "S", "D"))
print(Tie_Breaker(hand_1, hand_2, "Four of a Kind"))

# Test 2 - Player 2 Wins
hand_1 <- data.frame(rank = c(4, 4, 4, 4, 14), suit = c("H", "D", "C", "S", "H"))
hand_2 <- data.frame(rank = c(12, 12, 12, 12, 2), suit = c("H", "D", "C", "S", "D"))
print(Tie_Breaker(hand_1, hand_2, "Four of a Kind"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(7, 7, 7, 7, 13), suit = c("H", "D", "C", "S", "H"))
hand_2 <- data.frame(rank = c(7, 7, 7, 7, 13), suit = c("H", "D", "C", "S", "D"))
print(Tie_Breaker(hand_1, hand_2, "Four of a Kind"))


# 2.4 Full House
# Test 1 - Player 1 Wins
hand_1 <- data.frame(rank = c(9, 9, 9, 2, 2), suit = c("H", "D", "C", "H", "D"))
hand_2 <- data.frame(rank = c(8, 8, 8, 14, 14), suit = c("H", "D", "C", "S", "D"))
print(Tie_Breaker(hand_1, hand_2, "Full House"))

# Test 2 - Player 2 Wins
hand_1 <- data.frame(rank = c(6, 6, 6, 3, 3), suit = c("H", "D", "C", "H", "D"))
hand_2 <- data.frame(rank = c(6, 6, 6, 4, 4), suit = c("H", "D", "C", "C", "S"))
print(Tie_Breaker(hand_1, hand_2, "Full House"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(4, 4, 4, 13, 13), suit = c("H", "D", "C", "H", "D"))
hand_2 <- data.frame(rank = c(4, 4, 4, 13, 13), suit = c("H", "D", "S", "C", "S"))
print(Tie_Breaker(hand_1, hand_2, "Full House"))


# 2.5 Flush
# Test 1 - Player 1 Wins
hand_1 <- data.frame(rank = c(14, 13, 9, 6, 3), suit = c("H", "H", "H", "H", "H"))
hand_2 <- data.frame(rank = c(14, 12, 11, 7, 4),suit = c("S", "S", "S", "S", "S"))
print(Tie_Breaker(hand_1, hand_2, "Flush"))

# Test 2 - Player 2 Wins
hand_1 <- data.frame(rank = c(14, 11, 8, 6, 3), suit = c("D", "D", "D", "D", "D"))
hand_2 <- data.frame(rank = c(14, 12, 9, 7, 4), suit = c("C", "C", "C", "C", "C"))
print(Tie_Breaker(hand_1, hand_2, "Flush"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(14, 11, 9, 6, 3), suit = c("H", "H", "H", "H", "H"))
hand_2 <- data.frame(rank = c(3, 6, 9, 11, 14), suit = c("S", "S", "S", "S", "S"))
print(Tie_Breaker(hand_1, hand_2, "Flush"))


# 2.6 Straight
# Test 1 - Player 1 Wins
hand_1 <- data.frame(rank = c(10, 11, 12, 13, 14), suit = c("H", "S", "D", "C", "H"))
hand_2 <- data.frame(rank = c(9, 10, 11, 12, 13), suit = c("D", "C", "H", "S", "D"))
print(Tie_Breaker(hand_1, hand_2, "Straight"))

# Test 2 - Player 2 Wins
hand_1 <- data.frame(rank = c(14, 2, 3, 4, 5), suit = c("H", "S", "D", "C", "H"))
hand_2 <- data.frame(rank = c(2, 3, 4, 5, 6), suit = c("S", "D", "C", "H", "S"))
print(Tie_Breaker(hand_1, hand_2, "Straight"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(6, 7, 8, 9, 10), suit = c("H", "S", "D", "C", "H"))
hand_2 <- data.frame(rank = c(10, 9, 8, 7, 6), suit = c("S", "D", "C", "H", "S"))
print(Tie_Breaker(hand_1, hand_2, "Straight"))


# 2.7 Three of a Kind
# Test 1 - Player 1 Wins
hand_1 <- data.frame(rank = c(9, 9, 9, 2, 3), suit = c("H", "D", "C", "H", "S"))
hand_2 <- data.frame(rank = c(7, 7, 7, 14, 13), suit = c("H", "D", "C", "S", "D"))
print(Tie_Breaker(hand_1, hand_2, "Three of a Kind"))

# Test 2 - Player 2 Wins
hand_1 <- data.frame(rank = c(8, 8, 8, 13, 6), suit = c("H", "D", "C", "H", "S"))
hand_2 <- data.frame(rank = c(8, 8, 8, 14, 2), suit = c("H", "D", "C", "S", "D"))
print(Tie_Breaker(hand_1, hand_2, "Three of a Kind"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(5, 5, 5, 14, 10), suit = c("H", "D", "C", "H", "S"))
hand_2 <- data.frame(rank = c(5, 5, 5, 14, 10), suit = c("H", "D", "C", "S", "D"))
print(Tie_Breaker(hand_1, hand_2, "Three of a Kind"))


# 2.8 Two Pair
# Test 1 - Player 1 Wins
hand_1 <- data.frame(rank = c(12, 12, 3, 3, 2), suit = c("H", "D", "H", "D", "S"))
hand_2 <- data.frame(rank = c(11, 11, 10, 10, 14), suit = c("H", "D", "C", "S", "H"))
print(Tie_Breaker(hand_1, hand_2, "Two Pair"))

# Test 2 - Player 2 Wins
hand_1 <- data.frame(rank = c(13, 13, 4, 4, 14), suit = c("H", "D", "H", "D", "S"))
hand_2 <- data.frame(rank = c(13, 13, 7, 7, 2), suit = c("C", "S", "H", "D", "H"))
print(Tie_Breaker(hand_1, hand_2, "Two Pair"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(10, 10, 5, 5, 14), suit = c("H", "D", "H", "D", "C"))
hand_2 <- data.frame(rank = c(10, 10, 5, 5, 14), suit = c("C", "S", "C", "S", "D"))
print(Tie_Breaker(hand_1, hand_2, "Two Pair"))


# 2.9 One Pair
# Test 1 - Player 1 Wins
hand_1 <- data.frame(rank = c(13, 13, 14, 7, 2), suit = c("H", "D", "S", "C", "H"))
hand_2 <- data.frame(rank = c(10, 10, 14, 12, 9), suit = c("H", "D", "C", "S", "H"))
print(Tie_Breaker(hand_1, hand_2, "One Pair"))

# Test 2 - Player 2 Wins
hand_1 <- data.frame(rank = c(8, 8, 13, 10, 3), suit = c("H", "D", "S", "C", "H"))
hand_2 <- data.frame(rank = c(8, 8, 14, 9, 4), suit = c("C", "S", "D", "H", "S"))
print(Tie_Breaker(hand_1, hand_2, "One Pair"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(6, 6, 14, 11, 3), suit = c("H", "D", "S", "C", "H"))
hand_2 <- data.frame(rank = c(6, 6, 14, 11, 3), suit = c("C", "S", "D", "H", "S"))
print(Tie_Breaker(hand_1, hand_2, "One Pair"))


# 2.10 High Card
# Test 1 - Player 1 Wins
hand_1 <- data.frame(rank = c(14, 13, 9, 7, 3), suit = c("H", "S", "D", "C", "H"))
hand_2 <- data.frame(rank = c(14, 12, 11, 8, 4), suit = c("S", "D", "C", "H", "S"))
print(Tie_Breaker(hand_1, hand_2, "High Card"))

# Test 2 - Player 2 Wins
hand_1 <- data.frame(rank = c(14, 13, 11, 7, 3), suit = c("H", "S", "D", "C", "H"))
hand_2 <- data.frame(rank = c(14, 13, 12, 6, 2), suit = c("S", "D", "C", "H", "S"))
print(Tie_Breaker(hand_1, hand_2, "High Card"))

# Test 3 - Tie
hand_1 <- data.frame(rank = c(14, 12, 10, 8, 3), suit = c("H", "S", "D", "C", "H"))
hand_2 <- data.frame(rank = c(14, 12, 10, 8, 3), suit = c("S", "D", "C", "H", "S"))
print(Tie_Breaker(hand_1, hand_2, "High Card"))
