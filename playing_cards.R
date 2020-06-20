library(tidyverse)

cards <- read_csv("data/deck.csv")

deal <- function(df) {
    df[1, ]
}
deal(cards)

shuffle <- function(df) {
  random <- sample(1:52, size = 52, replace = FALSE)
  shuffled <- df[random, ]
  shuffled[1, ]
}

shuffle(cards)

cards_war <- cards

cards_war$value[cards$face == "ace"] <- 14

cards_heart <- cards

cards_heart$value <- 0
cards_heart$value[cards_heart$suit == "hearts"] <- 1
cards_heart$value[cards_heart$suit == "spades" & cards_heart$face == "queen"] <- 13

card_lackjack <- cards

card_lackjack$value[card_lackjack$face %in% c("jack", "queen", "king")] <- 10
card_lackjack$value[card_lackjack$face == "ace"] <- NA
