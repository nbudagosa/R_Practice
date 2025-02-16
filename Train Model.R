data <- mtcars
# train test split example
set.seed(42)
n <- nrow(data)
train_id <- sample(1:n, size = 0.8*n)
train_data <- data[train_id, ]
test_data <- data[-train_id, ]

# train linear regression
lm(y ~ x1 + x2 + x3, data = train_data)

# train logistic regression
glm(y ~ x1 + x2 + x3, data = train_data, family = "binomial")



# install packages
install.packages(c("rpart", "rpart.plot"))
library(rpart)
library(rpart.plot)

# train decision tree
treeModel <- rpart(y ~ x1 + x2 + x3, data = data, method = "class")

# plot decision tree (image shown below)
rpart.plot(treeModel)


# install packages
install.packages(("caret", "mlbench"))
library(caret)
library(mlbench)

# split data before training phase
# use the train test split code above

# train model with 5 folds cross validation
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

model <- train(diabetes ~ ., 
               data = PimaIndiansDiabetes,
               method = "rpart",
               trControl = ctrl)  

# model accuracy
p <- predict(model)
mean(p == PimaIndiansDiabetes$diabetes)

# confusion matrix
table(p, PimaIndiansDiabetes$diabetes, dnn = c("predicted", "actual"))

