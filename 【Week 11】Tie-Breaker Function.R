# 1.Tie-Breaker Function
# Input(hand_1, hand_2, score-hand card) --> Process(hand card --> compare the key numbers in the two hands) --> Output(win/tie)

# 2.Rank & Suit
# Rank: 2 < 3 < 4 < 5 < 6 < 7 < 8 < 10 < 11 < 12 < 13 < 14(A)
# Suit: Spade = Heart = Diamond = Club

# 3. Tie-Breaker Rule
# Royal Flush --> Win
# Straight Flush --> The greatest number in straight
# Four of a Kind --> Rank of 4 > Rank of 1
# Full House --> Rank of 3 > Rank of pair
# Flush --> The rank from the highest to lowest 
# Straight --> The greatest number in straight
# Three of a Kind --> Rank of 3 > Rank of 1 + 1
# Two Pair --> Rank of large Pair > Rank of small Pair > rank of 1
# One Pair --> Rank of Pair > Rank of 1 + 1 + 1
# High Card --> The rank from the highest to lowest 



###   Coding Part   ###

# 1.1 Define Function
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