# 1.1 Load the tidyverse package, which provides functions related to data processing
library(tidyverse) 


# 2.1 Creating 'Hearts'
hearts <- data.frame(rank = 2:14, suit = "heart") 
 # data.frame 数据框：rank列-数字、suit列-红桃

# 2.2 Creating 'Clubs'
clubs <- data.frame(rank = 2:14, suit = "club") 

# 2.3 Creating 'Spades'
spades <- data.frame(rank = 2:14,suit = "spade")

# 2.4 Creating 'Diamonds' 
diamonds <- data.frame(rank = 2:14, suit = "diamond") 


# 3.1 Combine to create Full Deck
deck <- rbind(hearts, clubs, spades, diamonds) 
 # rebind 按行拼接：列相同Suit和Rank，完整的表格


# 4.1 Define the Draw Function
draw <- function(deck, n)
 # deck-52 cards, n-number of drawn cards
{
 hand <- deck |> slice_sample(n=n) 
  # 从Deck中随机抽取n张牌，并将抽出的牌保存到hand
  # |> Pipe管道运算符：左边的数据传递给右边
  # hand <- slice_sample(deck, n=n)
 
 remaining_deck <- deck |> anti_join(hand)
  # 从原始牌库中移除已抽出的牌，得到剩余牌库
  # remaining_deck <- anti_join(deck, hand)
  # anti_join：保留第一个数据框中没有匹配到第二个数据框的行
  
 return(list(Hand = hand, Remaining_Deck = remaining_deck)) 
  # list：多个对象放置在同一个结构里
}


# 5.1 Test
draw(deck, 5)

