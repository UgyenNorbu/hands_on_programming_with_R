# SOT MACINE --------------------------------------------------------------

get_symbols <- function() {
    icons <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    sample(icons, size = 3, replace = TRUE, 
           prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}
a <- get_symbols()
score(a)
a
str(a)
score <- function(symbols){
    # Identify cases
    same <- length(unique(symbols)) == 1
    bars <- symbols %in% c("B", "BB", "BBB")
    
    # Calculate prize
    if (same) {
        payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                     "B" = 10,"C" = 10,"0" = 0)
        prize <- unname(payouts[symbols[1]]) #unname returns a copy of an object with the names attribute removed.
    } else if (all(bars)){
        prize <- 5
    } else {
        cherries <- sum(symbols == "C")
        payouts <- c(0, 2, 5)
        prize <- payouts[cherries + 1] # subsetting the vector 'payouts
    }
    
    # Adjust prize for diamonds
    diamond <- sum(symbols == "D")
    prize * (2 ^ diamond)
}

play <- function(){
    symbols <- get_symbols()
    prize <- score(symbols)
    structure(prize, symbols = symbols, class = "slots")
}

slot_display <- function(prize){ 
    # extract symbols
    symbols <- attr(prize, "symbol")
    
    # collapse symbols into single string
    symbols <- paste(symbols, collapse = " ")
    
    # combine symbol with prize as a regular expression \n is regular expression for new line (i.e. return or enter) 
    string <- paste(symbols, prize, sep = "\nBTN ")         
    
    # display regular expression in console without quotes
    cat(string)
}

# Using S3 method to print the desired slot_display without having to call it everytime
print.slots <- function(x, ...) { 
    slot_display(x)
}

# SIMULATION --------------------------------------------------------------
library(tidyverse)

wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")

combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1, 
          "B" = 0.25, "C" = 0.01, "0" = 0.52)
combos <- combos %>% 
    mutate(prob_1 = prob[Var1]) %>% 
    mutate(prob_2 = prob[Var2]) %>% 
    mutate(prob_3 = prob[Var3]) %>% 
    mutate(prob = prob_1*prob_2*prob_3) %>% 
    mutate(prize = NA)

for (i in 1:nrow(combos)){
    sym <-  c(combos[i, 1], combos[i, 2], combos[i, 3])
    combos$prize[i] <- score(sym)
}

combos <- combos %>% 
    mutate(exp_val = prob*prize)

sum(combos$exp_val)


# With Wild Card ----------------------------------------------------------

score_wild <- function(symbols) { 
    diamonds <- sum(symbols == "DD")
    cherries <- sum(symbols == "C")
    # identify case
    # since diamonds are wild, only nondiamonds matter for three of a kind and all bars 
    slots <- symbols[symbols != "DD"]
    same <- length(unique(slots)) == 1
    bars <- slots %in% c("B", "BB", "BBB")
    # assign prize
    if (diamonds == 3) { 
        prize <- 100
    } else if (same) {
        payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                     "B"=10,"C"=10,"0"=0) 
        prize <- unname(payouts[slots[1]])
    } else if (all(bars)) { 
        prize <- 5
    } else if (cherries > 0) {
        # diamonds count as cherries
        # so long as there is one real cherry
        prize <- c(0, 2, 5)[cherries + diamonds + 1]
    }else{ 
        prize <- 0
    }
    # double for each diamond
    prize * 2^diamonds
}

combos <- combos %>% 
    mutate(prize_wild  = NA)

for (i in 1:nrow(combos)){
    sym <-  c(combos[i, 1], combos[i, 2], combos[i, 3])
    combos$prize_wild[i] <- score_wild(sym)
}

combos <- combos %>% 
    mutate(exp_val_wild = prob*prize_wild)

sum(combos$exp_val_wild)
