library(tidyverse)

coin.flip <- function(n){
  
  flips <- rbinom(n = n, size = 1, prob = 0.5)
  outcome <- ifelse(flips == 1, yes = "Heads", no = "Tails")
  
  table(outcome) / n %>%
    return()
  
}



coin.flip.varied <- function(n){
  
  myprob <- runif(n = 1, min = 0.45, max = 0.55)
  
  flips <- rbinom(n = n, size = 1, prob = myprob)
  outcome <- ifelse(flips == 1, yes = "Heads", no = "Tails")
  
  table(outcome) %>%
    return()
}
