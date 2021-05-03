## load library
## install.packages(“mlbench”)
library(mlbench)
library(tidyverse)

## load dataset for regression
data("BostonHousing")
glimpse(BostonHousing)
View(BostonHousing)
## load dataset for classification

data(PimaIndiansDiabetes)
glimpse(PimaIndiansDiabetes)
# tibble == enhanced data frame
boston <- as_tibble(BostonHousing)

# preview/ explore data
head(boston)
tail(boston)

# split data
set.seed(42)
n <- nrow(boston)
id <- sample(1:n, size = n*0.8) # train ratio
train_data <- boston[id, ]
test_data <- boston[-id, ]

# train model
lmModel <- lm(medv ~ crim + rm + age,
               data = train_data)

# score model (prediction)
p <- predict(lmModel, newdata = test_data)

# evaluate model
error <- p - test_data$medv
rmse_test <- sqrt(mean(error**2))

