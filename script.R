library(ggplot2)
library(tidyverse)

roll <- function(die = 1:6) {
    dice <- sample(x = die, size = 2, replace = TRUE, 
                   prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
    sum(dice)
}
roll()

x <- as.data.frame(replicate(1000000, roll()))

x %>% 
    ggplot(aes(x = `replicate(1e+06, roll())`)) +
    geom_histogram(binwidth = 1)


roll_1 <- function(die = 1:6) {
    sample(x = die, size = 1, replace = TRUE, 
                   prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
}
roll_1()

x_1 <- as.data.frame(replicate(100000, roll_1()))

x_1 %>% 
    ggplot(aes(x = `replicate(1e+05, roll_1())`)) +
    geom_histogram(binwidth = 1)

colnames(x)
