# casino solution
# name: Kasidis S.
# date: 16 Jan 2021

# win chance 30% -> win 150 USD, cost 50 USD
# play 10 times -> total cost = 10 * 50 = 500 USD

casino_royal <- function() {
  # initialize prize vector 
  prize <- vector(mode = "numeric", length=10)
  
  # play games 10 times
  for (i in 1:10) {
    result <- sample( c("win", "loss"), size=1, prob = c(0.3, 0.7))
    if (result == "win") {
      prize[i] <- 150
    } else {
      prize[i] <- 0
    }
  }
  
  # return prize
  return(sum(prize))
}

#### REPLICATE - intro to simulation
mean(replicate(n = 10000, expr = casino_royal()))

# Thai population
# Mean Height = 168 cm, SD Height = 7 cm
# Question: How many Thai (%) that height < 158 cm? 

# Assume Height is normally distributed
heights <- rnorm(n = 100, mean = 168, sd = 7)

simulated_height_lt158 <- replicate(n = 10000, 
                                    mean(rnorm(n = 1000, mean = 168, sd = 7) < 158) )

mean(simulated_height_lt158)
