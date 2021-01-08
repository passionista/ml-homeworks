#Reading the data 
library(readr)
hw04_data_set <- read_csv("hw04_data_set.csv", 
                          col_names = TRUE)

#Dividing data into two as training and test
data_training <-as.matrix(hw04_data_set[1:150,])
data_test <- as.matrix(hw04_data_set[151:272,])

#Setting parameters
bin_width <- 0.37
origin_parameter <- 1.5
maximum_value <- 6

#Creating a data interval 
data_interval <- seq(from = origin_parameter, to = maximum_value, by = 0.01)

#Getting y values from the test data
y_test <- data_test[,2]

#Defining left and right borders of the window and assinging a value for each window
left_borders <- seq(from = origin_parameter, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = origin_parameter+bin_width, to = maximum_value, by = bin_width)

#  Regrossogram 

#Estimating y values from training data
g_head <- sapply(1:length(left_borders), function(b) {sum((left_borders[b] < data_training[,1] & data_training[,1] <= right_borders[b])*data_training[,2])/ sum(left_borders[ b] < data_training[,1] & data_training[,1] <= right_borders[b])})

#Drawing the plot
plot(data_training[,1],data_training[,2],type="p",col="blue",
     pch=19,las=1,lwd=0,main = sprintf("h = %g", bin_width),xlab = "Eruption time (min)",
     ylab = "Waiting time to next eruption (min)")
points(data_test[,1],data_test[,2],type="p",col="red",pch=19,las=1,lwd=0)
legend("topleft",
       c("training","test"),
       fill=c("blue","red"),
       bty="n")

for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(g_head[b], g_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(g_head[b], g_head[b + 1]), lwd = 2, col = "black") 
  }
}

#Calculating root mean square error for test data 
RMSE <-sqrt(sum(sapply(1:length(left_borders), function(b) {sum((data_test[(left_borders[b] < data_test[,1] & data_test[,1] <= right_borders[b]),2]- g_head[b])^2)}))/length(y_test))
sprintf("Regressogram => RMSE is %g when h is %g",RMSE ,bin_width)

 
# Running Mean Smoother

#Estimating y values from training data
g_head_rms <- sapply(data_interval, function(x) {sum(data_training[((x-0.5*bin_width) < data_training[,1] & data_training[,1] <= (x+0.5*bin_width)),2])/sum((x-0.5*bin_width) < data_training[,1] & data_training[,1] <= (x+0.5*bin_width))}) 

# Drawing the plot
plot(data_training[,1],data_training[,2],type="p",col="blue",
     pch=19,las=1,lwd=0,main = sprintf("h = %g", bin_width),xlab = "Eruption time (min)",
     ylab = "Waiting time to next eruption (min)")
points(data_test[,1],data_test[,2],type="p",col="red",pch=19,las=1,lwd=0)
lines(data_interval, g_head_rms, type = "l", lwd = 2, col = "black")

# Calculating RMSE 
RMSE_rms <- sqrt(sum(sapply(1:(length(data_interval)-1), function(b) {sum((data_test[(data_interval[b] < data_test[,1] & data_test[,1] <= data_interval[b+1]),2]- g_head_rms[b])^2)}))/length(y_test))
sprintf("Running Mean Smoother => RMSE is %g when h is %g",RMSE_rms,bin_width)

# Kernel Smoother

# Estimating y values from training data
g_head_kernel <- sapply(data_interval, function(x) {sum((1 / sqrt(2 * pi) * exp(-0.5 * ((x - data_training[,1]) / bin_width)^2)*data_training[,2]) / sum(1 / sqrt(2 * pi) * exp(-0.5 * ((x -data_training[,1]) / bin_width)^2)))})

# Drawing the plot
plot(data_training[,1],data_training[,2],type="p",col="blue",
     pch=19,las=1,lwd=0,main = sprintf("h = %g", bin_width),xlab = "Eruption time (min)",
     ylab = "Waiting time to next eruption (min)")
points(data_test[,1],data_test[,2],type="p",col="red",pch=19,las=1,lwd=0)
lines(data_interval, g_head_kernel, type = "l", lwd = 2, col = "black")

# Calculating RMSE 
RMSE_kernel <- sqrt(sum(sapply(1:(length(data_interval)-1), function(b) {sum((data_test[(data_interval[b] < data_test[,1] & data_test[,1] <= data_interval[b+1]),2]- g_head_kernel[b])^2)}))/length(y_test))
sprintf("Kernel Smoother => RMSE is %g when h is %g",RMSE_kernel ,bin_width)
