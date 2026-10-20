library(readxl)
library(tidyverse)
setwd("C:/Users/anna/GitHub/Training-Intuition-A-Poker-Hand-Simulator")
poker <- read_excel("poker_probabilities_comparison.xlsx")
poker_vis <- poker |>
  pivot_longer(
    cols = ends_with("(%)"),
    names_to = "num_cards",
    values_to = "probability"
  ) |>
  summarise(avg_prob = mean(probability)) |> 
  ggplot(aes(x=`Poker Hand`, y = avg_prob, fill = num_card)) +
  geom_col() +
  labs(
    title = 'Average Probability of Hand for 5-7 Card Draw',
    x = 'Hand Type',
    y = 'Average Probability'
  ) + 
  theme_bw()

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
'''