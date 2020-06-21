library(tidyverse)
library(devtools)

cards <- read_csv("data/deck.csv")

setup <- function(df){
    shuffle <- function(){
        random <- sample(1:52, size = 52)
        assign("cards", df[random, ], envir = globalenv())
    }
    
    deal <- function(df) {
        card <- df[1, ]
        assign("cards", df[-1, ], envir = globalenv())
        card
    }
    list(shuffle_out = shuffle, deal_out = deal)
}

deck <- setup(cards)
deal <- deck$deal_out
shuffle <- deck$shuffle_out
shuffle()
deal(cards)

