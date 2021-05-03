# caret template for building ml model
library(caret)

# regression model
# train control - kfold cross validation
set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)
# grid search
myGrid <- data.frame(k = c(5))

# train model
lmModel <- train(medv ~ . ,
                 data = train_data,
                 method = "knn",
                 #tuneLength = 4,
                 tuneGrid = myGrid,
                 trControl = ctrl)
lmModel

# score model (prediction)
p <- predict(lmModel, newdata = test_data)

# evaluate model -> rmse
error <- p - test_data$medv
rmse_test <- sqrt(mean(error**2))

rmse_test

