library(MASS)
library(onehot)
library(AUC)
library(cvAUC)
library(caret)
library(stats)
library(ggplot2)
library(ranger)
library(Metrics)

set.seed(421)

for(target in 1:3){
  #Getting the input
  X_train <- read.csv(sprintf("hw07_target%d_training_data.csv", target), header = TRUE)
  Y_train <- read.csv(sprintf("hw07_target%d_training_label.csv", target), header = TRUE)
  Y_train <- Y_train["TARGET"]
  X_test <- read.csv(sprintf("hw07_target%d_test_data.csv", target), header = TRUE)
  
  # Onehot encoding
  encoder <- onehot(X_train[,-1], addNA = TRUE, max_levels = Inf)
  X_train_encoder <- as.matrix(predict(encoder, data = X_train[,-1]))
  X_test_encoder <- as.matrix(predict(encoder, data = X_test[,-1]))

  
  # Assigning column means to NA values
  for(i in 1:ncol(X_train_encoder)){
    X_train_encoder[is.na(X_train_encoder[,i]), i] <- mean(X_train_encoder[,i], na.rm = TRUE)
  }
  for(i in 1:ncol(X_test_encoder)){
    X_test_encoder[is.na(X_test_encoder[,i]), i] <- mean(X_test_encoder[,i], na.rm = TRUE)
  }
  
  # Preprocessing by using PCA algorithm
  pre_process <- preProcess(X_train_encoder, method = c("center", "scale", "pca", "zv"), thresh = 0.95)
  X_train_p <- predict(pre_process,X_train_encoder)
  X_test_p <- predict(pre_process,X_test_encoder)
  
  #Fitting LDA and perform prediction 
  lda.fit = train(X_train_p,as.factor(Y_train$TARGET), method="lda")
  print(lda.fit)
  training_scores <- predict(lda.fit, X_train_p)
  as.numeric(levels(training_scores))[training_scores]
  
  # AUC score for training data
  print(auc(roc(predictions = training_scores, labels = as.factor(Y_train[, "TARGET"]))))
  
  #Prediction for test data and writing the result to another file
  test_scores <- predict(lda.fit, X_test_p)
  test_scores <- cbind(ID = X_test[,1], TARGET=as.vector(test_scores))
  write.table(test_scores, file = sprintf("hw07_target%d_test_predictions.csv", target), row.names = FALSE, sep = ",")
}
  

