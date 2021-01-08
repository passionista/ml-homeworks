library(readr)

# Read the data
cloth_image <- as.matrix(read_csv("hw02_images.csv", 
                        col_names = FALSE))
cloth_labels <- as.matrix(read_csv("hw02_labels.csv", 
                        col_names = FALSE))
initial_W <- as.matrix(read_csv("initial_W.csv", col_names = FALSE))
initial_w0 <- as.matrix(read_csv("initial_w0.csv", 
                       col_names = FALSE))

#Divide into two pieces astraining and test
cloth_image_training= as.matrix(cloth_image[1:500,])
cloth_image_test= as.matrix(cloth_image[501:1000,])
cloth_labels_training= as.vector(cloth_labels[1:500,])
cloth_labels_test= as.vector(cloth_labels[501:1000,])

# get the important values
ClothType <- max(cloth_labels)
LenLabel <- as.numeric(length(t(cloth_labels_training)))

# Sigmoid function
sigmoid <- function(X, W, w0){return (1 / (1 + exp(-((cbind(X, 1) %*% rbind(W, t(w0)))))))}

# Gradient functions
gradient_W <- function(X, Y_truth, Y_predicted) {
  return (-t (t((Y_truth-Y_predicted)*Y_predicted*(1-Y_predicted))%*%X))
}
gradient_w0 <- function(Y_truth, Y_predicted) {
  return (-colSums((Y_truth - Y_predicted)*Y_predicted*(1-Y_predicted)))
}

# One-of-N-coding
Y_truth <- matrix(0, LenLabel, ClothType)
Y_truth[cbind(1:LenLabel, cloth_labels_training)] <- 1  

Y_truth_test <- matrix(0, LenLabel, ClothType)
Y_truth_test[cbind(1:LenLabel, cloth_labels_training)] <- 1  

#Putting values given
iteration <- 1
eta <- 0.0001
epsilon <- 1e-3
W=initial_W
w0=initial_w0
#Creating objective values which are empty to add new values as iteration increases
objective_values <- c()
objective_values_test <- c()

# While loop to train W and w0 as iteration goes
while (1) {
  Y_predicted <- sigmoid(cloth_image_training, W, w0)
  
  objective_values <- c(objective_values, 1/2*sum(colSums((Y_truth - Y_predicted)^2)))
  
  
  W <- W -eta * gradient_W(cloth_image_training, Y_truth, Y_predicted)
  w0 <- w0 - eta * gradient_w0(Y_truth, Y_predicted)
  
  
  if (iteration>=500){
    break
  }
  
  iteration <- iteration + 1
}

#Drawing plot for error values
plot(1:(iteration), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

#Confusion matrix for training data
y_predicted <- apply(Y_predicted, 1 , which.max)
confusion_matrix <- table(y_predicted, cloth_labels_training)
print(confusion_matrix) 

#Multiplying W and w0 with test data
Y_predicted_test <- sigmoid(cloth_image_test, W, w0)

#Confusion matrix of test data
y_predicted_test <- apply(Y_predicted_test, 1 , which.max)
confusion_matrix_test <- table(y_predicted_test, cloth_labels_test)
print(confusion_matrix_test) 



