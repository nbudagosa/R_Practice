#library rpart.plot
library(rpart.plot)

# 2. train model
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE
)

# grid search
myGrid <- expand.grid(
  mtry = c(5, 10, 15, 20, 25),
  splitrule = c("gini", "extratrees"),
  min.node.size = 5
)

rfModel <- train(
  Class ~ . ,
  data = train_data,
  method = "ranger",
  importance = "impurity",
  metric = "Accuracy",
  trControl = ctrl,
  #tuneLength = 10
  tuneGrid = myGrid
)

# variable importance
varImp(rfModel)

