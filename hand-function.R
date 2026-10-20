library(readxl) # library needed to read in data
poker_probability <- function(hand) { # function that takes hand as arguement
  data <- read_excel("poker_probabilities_comparison.xlsx") |> 
    filter(`Poker Hand` == hand)  # filter for only probabilities for hand we want
  return(data$`5-Card Draw (%)`) # return probability for 5 card draw
}

# test cases
poker_probability("Royal Flush")
poker_probability(10)
