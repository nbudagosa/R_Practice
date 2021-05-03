# install packages
install.packages("MLmetrics")

# train model with prSummary
# precistion, recall, F1, AUC
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

# score model
p <- predict(knnModel, newdata = test_data)

# evaluate model
mean(p == test_data$Class)
cm1 <- table(p,
             test_data$Class,
             dnn = c(
               "Prediction",
               "Actual"
             ))

# confusionMatrix in caret
confusionMatrix(p, test_data$Class,
                positive= "M", # positive M , R 
                mode = "prec_recall")

