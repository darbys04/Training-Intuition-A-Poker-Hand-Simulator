library(readxl)
library(tidyverse)
setwd("C:/Users/anna/OneDrive/cycle")
poker <- read_excel("poker_probabilities_comparison.xlsx")
poker_vis <- poker |>
  pivot_longer(
    cols = ends_with("(%)"),
    names_to = "num_cards",
    values_to = "probability"
  ) |>
  group_by(across(all_of("Poker Hand"))) |> 
  summarise(avg_prob = mean(probability)) |> 
  ggplot(aes(x=reorder(`Poker Hand`, avg_prob), y = avg_prob)) +
  geom_col() +
  labs(
    title = 'Average Probability of Hand for 5-7 Card Draw',
    x = 'Hand Type',
    y = 'Average Probability'
  )
poker_vis  




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