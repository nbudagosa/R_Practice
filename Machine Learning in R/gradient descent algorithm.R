## gradient descent algorithm
head(mtcars)

## Our model: mpg = 37.285 - 5.344*wt 
#(Linear regression Model)
lm(mpg ~ wt, data = mtcars)

## create our own GD algorithm
gradientDescent <- function(data, x, y, alpha=0.05, iter=10000) {
  ## randomize b0, b1
  b0 <- runif(1)
  b1 <- runif(1)
  
  ## get n, x, y
  n <- nrow(data)
  x <- zscore(data[[x]])
  y <- zscore(data[[y]])
  
  ## update b0 and b1, loop iteration 10000
  for (i in 1:iter) {
    new_b0 <- b0 - alpha * (1/n) * sum(b0 + b1*x - y)
    new_b1 <- b1 - alpha * (1/n) * sum((b0 + b1*x - y) * x)
    b0 <- new_b0
    b1 <- new_b1
  }
  ## return intercept and slope
  list(intercept = b0, slope = b1) 
}

## create Z-Score function (scale())
## standardization
zscore <- function(x) {
  (x - mean(x)) / sd(x)
}

zscore(x = c(10, 25, 30, 100, 150))
scale(x = c(10, 25, 30, 100, 150))
