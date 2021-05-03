#load library
library(caret)
library(tidyverse)
library(mlbench)
install.packages('e1071', dependencies=TRUE)

# load dataset
data("Sonar")

# preview dataset
View(Sonar)
glimpse(Sonar)

# check completeness?
mean(complete.cases(Sonar))

# review column Class
table(Sonar$Class)

# -----------------------------------
# -- TRAIN MODEL ---------------------
# 1. split data
set.seed(42)
train_id <- createDataPartition(
  Sonar$Class,
  p = 0.8,
  list = FALSE
)
train_data <- Sonar[train_id, ]
test_data <- Sonar[-train_id, ]

# 2. train model
set.seed(42)
ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

knnModel <- train(
  Class ~ . ,
  data = train_data,
  method = "knn",
  metric = "Accuracy",
  trControl = ctrl
)

treeModel <- train(
  Class ~ .,
  data = train_data,
  method = "rpart",
  metric = "Accuracy",
  trControl = ctrl
)
# Winning Model
randomForestModel <- train(
  Class ~ . ,
  data = train_data,
  method = "rf",
  metric = "Accuracy",
  trControl = ctrl
)



# 3. score model (prediction)
p <- predict(knnModel, newdata = test_data)
p2 <- predict(treeModel, newdata = test_data)
p3 <- predict(randomForestModel,
              newdata = test_data)

# 4. evaluate model// default Accuracy
mean(p == test_data$Class)
mean(p2 == test_data$Class)
mean(p3 == test_data$Class)

# 5. moodel comparison
modelList <- list(
  knn = knnModel,
  decisionTree = treeModel,
  randomForest = randomForestModel
)

result <- resamples(modelList)
summary(result)

