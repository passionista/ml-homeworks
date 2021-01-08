library(randomForest)

#Getting the input 
X_train <-read.csv("training_data.csv", header = TRUE)
y_train <- X_train[,7]
X_test<- read.csv("test_data.csv", header = TRUE)

#Sorting the data according to date
sorted_x <- X_train[order((X_train[,5])),]
sorted_x <- sorted_x[order((sorted_x[,4])),]
sorted_x <- sorted_x[order((sorted_x[,3])),]
sorted_x <- sorted_x[order((sorted_x[,4])),]

#Seperating training data as train and validation
test_data1 <- which(sorted_x[,4]==12,X_train[,3]>11) 
TrainSet1 <- sorted_x[-test_data1,] # data until 11 December
ValidSet1 <- sorted_x[test_data1,] # data after 11 December 

#Train the random forest by using train data and predict validation data (it takes approximately 1 minute due to size of the data)
model1 <- randomForest(TRX_COUNT~.,TrainSet1[,1:7],mtry=3 , ntree=100,importance = TRUE)
predicted1 <- predict(model1,ValidSet1[,1:6])

#Checking errors
mean(abs(predicted1 -ValidSet1[,7] )) 
sqrt(mean((predicted1 -ValidSet1[,7] )^2)) 

plot(model1)


#Applying same procedure by adding the data of 10 more days
train_data2 <- which(sorted_x[,4]==3,X_train[,3]<=11) 
test_data2 <- which(sorted_x[,4]==12,X_train[,3]>21) 


TrainSet1 <- sorted_x[-test_data2,]
TrainSet2 <- TrainSet1[-train_data2,] #data between 11 March and 21 December
ValidSet2 <- sorted_x[test_data2,] # data after 21 December

model2 <- randomForest(TrainSet2[,1:6],TrainSet2[,7], mtry=3 , ntree=100, importance = TRUE)
predicted2 <- predict(model2,ValidSet2[,1:6]) 

mean(abs(predicted2 -ValidSet2[,7] )) 
sqrt(mean((predicted2 -ValidSet2[,7] )^2))


# Applying the same procedure for future prediction by sliding my training data 10 days more. 
train_data3 <- which(sorted_x[,4]==3,X_train[,3]<=21) 

TrainSet3 <- sorted_x[-train_data3,]# data after 21 March 

model3 <- randomForest(TrainSet3[,1:6],TrainSet3[,7],  mtry=3 , ntree=100, importance = TRUE)
future_prediction <- predict(model3,X_test[,1:6])

write.table(future_prediction, file = "test_predictions.csv", row.names = FALSE, col.names ='Predictions')

# To see the future importance of features, best mtry and best ntree. ( This takes 12 minutes to perform, I used this only to show how I obtained the plot.)
metric<-'RMSE'

customRF <- list(type = "Regression", library = "randomForest", loop = NULL)

customRF$parameters <- data.frame(parameter = c("maxnodes", "ntree"), class = rep("numeric", 2), label = c("maxnodes", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, maxnodes = param$maxnodes, ntree=param$ntree, ...)
}

customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

control <- trainControl(method="repeatedcv", number=10, repeats=3, search='grid')
tunegrid <- expand.grid(.maxnodes=c(2,3,5), .ntree=c(100))
set.seed(7)

rf_gridsearch <- train(x=X_train[,1:6], y=X_train[,7], method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)

plot(rf_gridsearch)
rf_gridsearch$bestTune
varImpPlot(rf_gridsearch$finalModel, main ='Feature importance')

