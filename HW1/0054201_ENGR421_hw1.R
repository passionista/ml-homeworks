library(readr)
images <- read_csv("Desktop/ENGR421/engr421_dasc52_fall2019_hw01(1)/hw01_images.csv", colnames=TRUE)
labels <- read_csv("Desktop/ENGR421/engr421_dasc521_fall2019_hw01(1)/hw01_labels.csv", 
                        col_names = FALSE)

# Data is divided into data set as training and test
x_training <- images[1:200, ]
y_training <- labels[1:200, ]
x_test <- images[201:400, ]
y_test <- labels[201:400, ]

#Get the class size and lenght of data set 
GenderType <- max(labels)
LenY <- length(labels)

#Calculate means for each classes
means_training <- sapply(X = 1:GenderType, FUN = function(c) {colMeans(x_training[y_training == c,])})
#Calculate standard deviations for each classes
sample_deviations <- sapply(X = 1:GenderType, FUN = function(c) {sqrt(colMeans((x_training[y_training == c,] - means_training[c,])^2))}) 
#Calculate class probabilities for each classes
class_prob <- sapply(X = 1:GenderType, FUN = function(c) {mean(y_test == c)})

#Calculate score values for each classes
score_values_tr1 <- colSums(t(- 0.5 * log(2 * pi * sample_deviations[,1]^2) - 0.5 * (x_training - means_training[,1])^2 / sample_deviations[,1]^2))+ log(class_prob[1])
score_values_tr2 <- colSums(t(- 0.5 * log(2 * pi * sample_deviations[,2]^2) - 0.5 * (x_training - means_training[,2])^2 / sample_deviations[,2]^2))+ log(class_prob[2])
#Bring score values together in a matrix
score_values= cbind(score_values_tr1,score_values_tr2)
#Assigning to classes
class_assignments_1 <- apply(X = score_values, MARGIN = 1, FUN = which.max)


#Confusion matrix for training data
y_predicted <- 1 * ((class_assignments_1) > 1)
confusion_matrix <- table(y_predicted, t(y_training))


#Applied same calculations for test data
score_values_tst1 <- colSums(t(- 0.5 * log(2 * pi * sample_deviations[,1]^2) - 0.5 * (x_test - means_training[,1])^2 / sample_deviations[,1]^2))+ log(class_prob[1])
score_values_tst2 <- colSums(t(- 0.5 * log(2 * pi * sample_deviations[,2]^2) - 0.5 * (x_test - means_training[,2])^2 / sample_deviations[,2]^2))+ log(class_prob[2])
score_values_test= cbind(score_values_tst1,score_values_tst2)
class_assignments_2 <- apply(X = score_values_test, MARGIN = 1, FUN = which.max)

#Confusion matrix for test data
y_predicted2 <- 1 * ((class_assignments_2) > 1)
confusion_matrix2 <- table(y_predicted2+1, t(y_test))



