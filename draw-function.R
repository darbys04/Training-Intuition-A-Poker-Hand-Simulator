library(tidyverse) #libraries

hearts <- data.frame(
  rank = 2:14,
  suit = "heart" 
) # created hearts df
clubs <- data.frame(
  rank = 2:14,
  suit = "club" 
) # clubs df
spades <- data.frame(
  rank = 2:14,
  suit = "spade" 
) # spades df
diamonds <- data.frame(
  rank = 2:14,
  suit = "diamond" 
) # diamonds
deck <- rbind(hearts, clubs, spades, diamonds) # combined suits to create full deck

draw <- function(deck, n) { # function with deck and number of cards as arguement
  hand <- deck |>  
    slice_sample(n=n) # sampled n number of cards from deck
  remaining_deck <- deck |> 
    anti_join(hand) # created df of deck excluding rows (cards) from hand
  return(list(
    Hand = hand,
    Remaining_Deck = remaining_deck
  )) # returned hand and remaining deck
}

draw(deck, 5) # test case