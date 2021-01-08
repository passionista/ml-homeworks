library(AUC)
library(onehot)
library(xgboost)
library(tree)
library(randomForest)
library(car)
library(auRoc)
library(pROC)
library(ROCR)

# Reading the input files
X_train <- read.csv("hw08_training_data.csv", header = TRUE)
Y_train <- read.csv("hw08_training_label.csv", header = TRUE)
X_test <- read.csv("hw08_test_data.csv", header = TRUE)

#Encoding for categorical features
encoder <- onehot(X_train, addNA = TRUE, max_levels = Inf)
X_train_d <- predict(encoder, data = X_train)
X_test_d <- predict(encoder, data = X_test)

#Setting seed for randomness
set.seed(421)

#Generating a new matrix with 7 columns for the predictions 
test_predictions <- matrix(0, nrow = nrow(X_test_d), ncol = ncol(Y_train))
colnames(test_predictions) <- colnames(Y_train)
test_predictions[,1] <- X_test[, 1]

#Generating a for loop for each column in the Y_train except for first column
for (outcome in 1:6) {
  #Choosing the customers who has label for the cloumn 'outcome'
  valid_customers <- which(is.na(Y_train[,outcome + 1]) == FALSE)
  
  #Getting indicies for customers labeled as 0 and 1 for the column 'outcome' 
  valid_customers_0<- which(Y_train[,outcome + 1]==0)
  valid_customers_1 <- which(Y_train[,outcome + 1]==1)
  
  #Generating train set by choosing 90% of customers labeled as 0 and 1 separately
  train_set_0 <- sample (valid_customers_0, floor(length(valid_customers_0)*0.9))
  train_set_1 <- sample (valid_customers_1, floor(length(valid_customers_1)*0.9))
  new_train_set= c(train_set_0,train_set_1)
  
  #Shuffle the new train set to avoid of biases during training 
  shuffled_train=sample(new_train_set,length(new_train_set))
  
  #Training boosting model by using 90% of our data and their label
  boosting_model <- xgboost(data = X_train_d[shuffled_train, -1], label = Y_train[shuffled_train, outcome + 1], nrounds = 20, objective = "binary:logistic")
  #Predicting the other 10% of our data by using boosting model 
  training_scores <- predict(boosting_model, X_train_d[-shuffled_train, -1])
  
  # AUC score for validation data
  auc(roc(Y_train[-shuffled_train, outcome + 1],training_scores))
  
  #Predicting the test data by using same boosting model above
  test_scores <- predict(boosting_model, X_test_d[, -1])
  test_predictions[, outcome + 1] <- test_scores
}
#Writing the predictions into different csv file
write.table(test_predictions, file = "hw08_test_predictions.csv", row.names = FALSE, sep = ",")


# Execute this code to see explicitly AUC value of validation set for the second column
outcome=1
valid_customers <- which(is.na(Y_train[,outcome + 1]) == FALSE)

valid_customers_0<- which(Y_train[,outcome + 1]==0)
valid_customers_1 <- which(Y_train[,outcome + 1]==1)

train_set_0 <- sample (valid_customers_0, floor(length(valid_customers_0)*0.9))
train_set_1 <- sample (valid_customers_1, floor(length(valid_customers_1)*0.9))

new_train_set= c(train_set_0,train_set_1)
shuffled_train=sample(new_train_set,length(new_train_set))

boosting_model <- xgboost(data = X_train_d[shuffled_train, -1], label = Y_train[shuffled_train, outcome + 1], nrounds = 20, objective = "binary:logistic")
training_scores <- predict(boosting_model, X_train_d[-shuffled_train, -1])

# AUC score for training data
auc(roc(Y_train[-shuffled_train, outcome + 1],training_scores))

# Execute this code to see explicitly AUC value of validation set for the third column
outcome=2
  
valid_customers <- which(is.na(Y_train[,outcome + 1]) == FALSE)

valid_customers_0<- which(Y_train[,outcome + 1]==0)
valid_customers_1 <- which(Y_train[,outcome + 1]==1)

train_set_0 <- sample (valid_customers_0, floor(length(valid_customers_0)*0.9))
train_set_1 <- sample (valid_customers_1, floor(length(valid_customers_1)*0.9))

new_train_set= c(train_set_0,train_set_1)
shuffled_train=sample(new_train_set,length(new_train_set))

boosting_model <- xgboost(data = X_train_d[shuffled_train, -1], label = Y_train[shuffled_train, outcome + 1], nrounds = 20, objective = "binary:logistic")
training_scores <- predict(boosting_model, X_train_d[-shuffled_train, -1])

# AUC score for training data
auc(roc(Y_train[-shuffled_train, outcome + 1],training_scores))


# Execute this code to see explicitly AUC value of validation set for the forth column
outcome=3

valid_customers <- which(is.na(Y_train[,outcome + 1]) == FALSE)

valid_customers_0<- which(Y_train[,outcome + 1]==0)
valid_customers_1 <- which(Y_train[,outcome + 1]==1)

train_set_0 <- sample (valid_customers_0, floor(length(valid_customers_0)*0.9))
train_set_1 <- sample (valid_customers_1, floor(length(valid_customers_1)*0.9))

new_train_set= c(train_set_0,train_set_1)
shuffled_train=sample(new_train_set,length(new_train_set))

boosting_model <- xgboost(data = X_train_d[shuffled_train, -1], label = Y_train[shuffled_train, outcome + 1], nrounds = 20, objective = "binary:logistic")
training_scores <- predict(boosting_model, X_train_d[-shuffled_train, -1])

# AUC score for training data
auc(roc(Y_train[-shuffled_train, outcome + 1],training_scores))

# Execute this code to see explicitly AUC value of validation set for the fifth column
outcome=4

valid_customers <- which(is.na(Y_train[,outcome + 1]) == FALSE)

valid_customers_0<- which(Y_train[,outcome + 1]==0)
valid_customers_1 <- which(Y_train[,outcome + 1]==1)

train_set_0 <- sample (valid_customers_0, floor(length(valid_customers_0)*0.9))
train_set_1 <- sample (valid_customers_1, floor(length(valid_customers_1)*0.9))

new_train_set= c(train_set_0,train_set_1)
shuffled_train=sample(new_train_set,length(new_train_set))

boosting_model <- xgboost(data = X_train_d[shuffled_train, -1], label = Y_train[shuffled_train, outcome + 1], nrounds = 20, objective = "binary:logistic")
training_scores <- predict(boosting_model, X_train_d[-shuffled_train, -1])

# AUC score for training data
auc(roc(Y_train[-shuffled_train, outcome + 1],training_scores))

# Execute this code to see explicitly AUC value of validation set for the sixth column
outcome=5

valid_customers <- which(is.na(Y_train[,outcome + 1]) == FALSE)

valid_customers_0<- which(Y_train[,outcome + 1]==0)
valid_customers_1 <- which(Y_train[,outcome + 1]==1)

train_set_0 <- sample (valid_customers_0, floor(length(valid_customers_0)*0.9))
train_set_1 <- sample (valid_customers_1, floor(length(valid_customers_1)*0.9))

new_train_set= c(train_set_0,train_set_1)
shuffled_train=sample(new_train_set,length(new_train_set))

boosting_model <- xgboost(data = X_train_d[shuffled_train, -1], label = Y_train[shuffled_train, outcome + 1], nrounds = 20, objective = "binary:logistic")
training_scores <- predict(boosting_model, X_train_d[-shuffled_train, -1])

# AUC score for training data
auc(roc(Y_train[-shuffled_train, outcome + 1],training_scores))

# Execute this code to see explicitly AUC value of validation set for the seventh column
outcome=6

valid_customers <- which(is.na(Y_train[,outcome + 1]) == FALSE)

valid_customers_0<- which(Y_train[,outcome + 1]==0)
valid_customers_1 <- which(Y_train[,outcome + 1]==1)

train_set_0 <- sample (valid_customers_0, floor(length(valid_customers_0)*0.9))
train_set_1 <- sample (valid_customers_1, floor(length(valid_customers_1)*0.9))

new_train_set= c(train_set_0,train_set_1)
shuffled_train=sample(new_train_set,length(new_train_set))

boosting_model <- xgboost(data = X_train_d[shuffled_train, -1], label = Y_train[shuffled_train, outcome + 1], nrounds = 20, objective = "binary:logistic")
training_scores <- predict(boosting_model, X_train_d[-shuffled_train, -1])

# AUC score for training data
auc(roc(Y_train[-shuffled_train, outcome + 1],training_scores))
