get_symbols <- function() {
    icons <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    sample(icons, size = 3, replace = TRUE, 
           prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

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

play()