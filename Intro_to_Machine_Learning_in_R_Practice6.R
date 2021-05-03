# train model with twoClassSummary
# precistion, recall, F1, AUC
set.seed(42)
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  verboseIter = TRUE,
  summaryFunction =  twoClassSummary,
  classProbs = TRUE
)

randomForestModel <- train(
  Class ~ . ,
  data = train_data,
  method = "rf", # random forest
  metric = "ROC",
  trControl = ctrl
)

# score model
p <- predict(randomForestModel,
             newdata = test_data)

# evaluate model
mean(p == test_data$Class)

# manual confusion matrix
cm1 <- table(p, test_data$Class,
             dnn = c("Prediction", "Actual"))

confusionMatrix(p, test_data$Class,
                positive = "M")

