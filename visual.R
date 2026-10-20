library(readxl)
library(tidyverse) # read in libraies
setwd("C:/Users/anna/GitHub/Training-Intuition-A-Poker-Hand-Simulator") # set working directory
poker <- read_excel("poker_probabilities_comparison.xlsx") # read in data file
poker_vis <- poker |> 
  pivot_longer( 
    cols = ends_with("(%)"),
    names_to = "num_cards",
    values_to = "probability"
  ) |> # rearrange data so the number of cards and the probability of each are their own variables
  ggplot(aes(x=reorder(`Poker Hand`, probability), y = probability, fill = num_cards)) +
  geom_col() + # create bar chart of probabilities for each hand
  labs(
    title = 'Probability of Hand for 5-7 Card Draw',
    x = 'Hand Type',
    y = 'Probability'
  ) + #add labels
  theme_bw() # add theme 

poker_vis  



'''
poker_vis <- poker |>
  pivot_longer(
    cols = ends_with("(%)"),
    names_to = "num_cards",
    values_to = "probability"
  ) |>
  ggplot() +
  geom_col(aes(x=`Poker Hand`, y = probability, fill = num_cards),  position = "dodge") +
  labs(
    title = 'Probability of Hand for 5-7 Card Draw',
    x = 'Hand Type',
    y = 'Average Probability',
    fill = 'Number of Cards'
  ) +
  theme_classic()
poker_vis  