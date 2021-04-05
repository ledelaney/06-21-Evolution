library(tidyverse)

# Drift in one generation
coin.flip.random <- function(n){
  
  flips <- rbinom(n = n, size = 1, prob = 0.5)
  outcome <- ifelse(flips == 1, yes = "Heads", no = "Tails")
  
  table(outcome) / n %>%
    return()
  
}

# Strength of "drift" decreases as sample size increases
coin.flip.random(10)
coin.flip.random(100)
coin.flip.random(1000)
coin.flip.random(10000)



# Selection in one generation
coin.flip.nonrandom <- function(n, prob){
  
  flips <- rbinom(n = n, size = 1, prob = prob)
  outcome <- ifelse(flips == 1, yes = "Heads", no = "Tails")
  
  table(outcome) / n %>%
    return()
  
}

# Slightly favoring heads in the next generation
coin.flip.nonrandom(10, 0.52)
coin.flip.nonrandom(100, 0.52)
coin.flip.nonrandom(1000, 0.52)
coin.flip.nonrandom(10000, 0.52)
coin.flip.nonrandom(100000, 0.52)

# If this "favor" was inherited by the next generation...
# Lead to more and more heads!


coin.flip.varied <- function(n){
  
  myprob <- runif(n = 1, min = 0.45, max = 0.55)
  
  flips <- rbinom(n = n, size = 1, prob = myprob)
  outcome <- ifelse(flips == 1, yes = "Heads", no = "Tails")
  
  table(outcome) %>%
    return()
}
