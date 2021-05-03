#load library
library(caret)
library(tidyverse)
library(mlbench)
install.packages('e1071', dependencies=TRUE)

# load dataset
data(Sonar)

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

# 5. moodel comparison
modelList <- list(
  knn = knnModel,
  decisionTree = treeModel,
  randomForest = randomForestModel
)

result <- resamples(modelList)
summary(result)

# 3. score model (prediction)
p3 <- predict(randomForestModel,
              newdata = test_data)

# 4. evaluate model// default Accuracy
mean(p3 == test_data$Class)

# table(preds, actuals)
cm1 <- table(p3,
             test_data$Class,
             dnn = c("Prediction", "Actual"))

# accuracy
(20 + 13) / (20 + 6 + 2 + 13)
sum(diag(cm1)) / sum(cm1)

# precision => 'M' positive class
20 / (20 + 6)
cm1[1, 1] / sum(cm1[1, ])

# recall => 'M' positive class
20 / (20 + 2)
rc <- cm1[1, 1] / sum(cm1[, 1])

# F1 score
# F1 = 2*pr*rc / (pr + rc)
f1 <- 2*pr*rc / (pr + rc)
print(f1)

## ------------------------------------
## Confusion Matrix (Binary Classification)
confusionMatrix(p3, text_data$Class,
                positive = "R") # positive R,M


