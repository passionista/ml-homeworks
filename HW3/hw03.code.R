#Safelog function to prevent minus values in log function
safelog <- function(x) {
  return (log(x + 1e-100))
}

library(readr)
# Implement the data given
cloth_image <- as.matrix(read_csv("hw03_images.csv", 
                                  col_names = FALSE))
cloth_labels <- as.matrix(read_csv("hw03_labels.csv", 
                                   col_names = FALSE))

initial_W <- as.matrix(read_csv("initial_W.csv", 
                                col_names = FALSE))
initial_V <- as.matrix(read_csv("initial_V.csv", 
                                col_names = FALSE))

#Divide into two pieces as training and test
cloth_image_training= as.matrix(cloth_image[1:500,])
cloth_image_test= as.matrix(cloth_image[501:1000,])
cloth_labels_training= as.vector(cloth_labels[1:500,])
cloth_labels_test= as.vector(cloth_labels[501:1000,])

# Get the important values
N <- as.numeric(length(cloth_labels_training))
ClothType <- max(cloth_labels)

# Sigmoid function
sigmoid <- function(a) {
  b=1 / (1 + exp(-a))
  return (b) }

# Softmax function 
softmax <- function(a) {
  scores <- a
  scores <- exp(scores - matrix(apply(scores, MARGIN = 1, FUN = max), nrow = nrow(scores), ncol = ncol(scores), byrow = FALSE))
  scores <- scores / matrix(rowSums(scores), nrow(scores), ncol(scores), byrow = FALSE)
  return (scores)
}

# Set learning parameters
eta <- 0.0005
epsilon <- 1e-3
H <- 20
max_iteration <- 500


# One-of-N-coding
Y_truth_training <- matrix(0, N, ClothType)
Y_truth_training[cbind(1:N, cloth_labels_training)] <- 1 

#First objective value
Z <- sigmoid(cbind(1, cloth_image_training) %*% initial_W)
y_predicted <- as.matrix(softmax(cbind(1, Z) %*% initial_V))
objective_values <- -sum(Y_truth_training * safelog(y_predicted))

# Set values
iteration <- 1
W=initial_W
v=initial_V

# Learn W and v using gradient descent and batch learning
while (iteration<max_iteration) {
    
    delta_v <- eta * (t(Z)%*%(Y_truth_training- y_predicted)) 
    delta_W <- eta * t(cbind(1,cloth_image_training)) %*% ((Y_truth_training- y_predicted) %*% t(v[2:(H + 1),]) * (Z) * (1-Z)) 

  #Add 0 infront of delta_v to make it conformable with v
    v <- v + rbind(0,delta_v)
    W <- W + delta_W
 
  
  Z <- sigmoid(cbind(1, cloth_image_training) %*% W)
  y_predicted <- softmax(cbind(1, Z) %*% v)
  objective_values <- c(objective_values, -sum(Y_truth_training * safelog(y_predicted)))
  
  iteration <- iteration + 1
  
  if (iteration>max_iteration){
    break
  }}

# Plot objective function for training data
plot(1:(iteration), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

# calculate confusion matrix for training data
Y_predicted <- apply(y_predicted, 1 , which.max)
confusion_matrix <- table(Y_predicted, cloth_labels_training)
print(confusion_matrix)


#Predicting the classes of test data by using new v and W
Z2 <- sigmoid(cbind(1, cloth_image_test) %*% W)
Y_predicted_test<- as.matrix(softmax(cbind(1, Z2) %*% v))

#Confusion matrix for test data
y_predicted_test <- apply(Y_predicted_test, 1 , which.max)
confusion_matrix_test <- table(y_predicted_test, cloth_labels_test)
print(confusion_matrix_test) 

