library(readr)
data_set <- read_csv("Desktop/Fall 2019/ENGR421/Homeworks/HW5/engr421_dasc521_fall2019_hw05/hw05_data_set.csv")

x_train= as.vector(data_set[1:150,1])
y_train= as.vector(data_set[1:150,2])
x_test= as.vector(data_set[151:272,1])
y_test= as.vector(data_set[151:272,2])

P=25

N_train <-as.numeric( length(t(y_train)))
N_test <- as.numeric(length(t(y_test)))

node_indices <- list()
is_terminal <- c()
need_split <- c()

node_features <- c()
node_splits <- c()
node_frequencies <- list()

node_indices <- list(1:N_train)
is_terminal <- c(FALSE)
need_split <- c(TRUE)

        
while (1) {
        # find nodes that need splitting
        split_nodes <- which(need_split)
        # check whether we reach all terminal nodes
        if (length(split_nodes)==0) {
                break
        }
        # find best split positions for all nodes
        for (split_node in split_nodes) {
                data_indices <- node_indices[[split_node]]
                need_split[split_node] <- FALSE
                node_frequencies[[split_node]] <- length(data_indices)  
                
                # check whether node is pure
                if (length(data_indices)<=25) {
                        is_terminal[split_node] <- TRUE
                } else {
                        is_terminal[split_node] <- FALSE
                        
                       #sort our unique x's in our data points 
                        x_train_sort <- t(sort(t(unique(x_train[data_indices,]))))
                        best_scores <- rep(0, length(x_train_sort))
                        best_splits <- rep(0,  length(x_train_sort))
                        split_positions <- (x_train_sort[-1] + x_train_sort[-length(x_train_sort)]) / 2
                        split_scores <- rep(0, length(split_positions))
                        
                        # Trying all the possible splits between our unique x's
                                for (s in 1:length(split_positions))  {
                                
                                        left_indices <- data_indices[(x_train[data_indices,] < split_positions[s])]
                                        right_indices <- data_indices[(x_train[data_indices,] >= split_positions[s])]
                                        mean_left=sum(y_train[left_indices,])/length(left_indices)
                                        mean_right=sum(y_train[right_indices,])/length(right_indices)
                                        split_scores[s] <- (sum((as.vector(y_train[left_indices,])-mean_left)^2) + sum((as.vector(y_train[right_indices,])-mean_right)^2))/ length(data_indices)
                                }
                                #choosing the best score among all possible splits 
                                best_scores <- min(split_scores[1:length(split_positions)])
                                best_splits<- split_positions[which.min(split_scores[1:length(split_positions)])]
                        
                        # decide where to split on which feature
                        split_d <- which.min(split_scores[1:length(split_positions)])
                        node_features[split_node] <- split_d
                        node_splits[split_node] <- best_splits
                        
                        # create left node using the selected split
                        left_indices <- data_indices[(x_train[data_indices,] < split_positions[split_d])]
                        node_indices[[2 * split_node]] <- left_indices
                        is_terminal[2 * split_node] <- FALSE
                        need_split[2 * split_node] <- TRUE
                        
                        # create left node using the selected split
                        right_indices <- data_indices[(x_train[data_indices,] >= split_positions[split_d])]
                        node_indices[[2 * split_node + 1]] <- right_indices
                        is_terminal[2 * split_node + 1] <- FALSE
                        need_split[2 * split_node + 1] <- TRUE
                  }
                }
}
# extract rules
terminal_nodes <- which(is_terminal)
for (terminal_node in terminal_nodes) {
        index <- terminal_node
        rules <- c()
        while (index > 1) {
                parent <- floor(index / 2)
                if (index %% 2 == 0) {
                        # if node is left child of its parent
                        rules <- c(sprintf("x%d < %g", node_features[parent], node_splits[parent]), rules)
                } else {
                        # if node is right child of its parent
                        rules <- c(sprintf("x%d >= %g", node_features[parent], node_splits[parent]), rules)
                }
                index <- parent
        }
        print(sprintf("{%s} => [%s]", paste0(rules, collapse = " AND "), paste0(node_frequencies[[terminal_node]], collapse = "-")))
}

# traverse tree for test data points
data_interval <- seq(from =1.5, to = 6, by = 0.01)
y_predicted <- rep(0, length(data_interval))
for (i in 1:length(data_interval)) {
        index <- 1
        while (1) {
                if (is_terminal[index] == TRUE) {
                        y_predicted[i] <- sum(as.vector(y_train[node_indices[[index]],]))/length(t(y_train[node_indices[[index]],]))
                        break
                } else {
                        if (data_interval[i] < node_splits[index])  {
                                index <- index * 2
                        } else {
                                index <- index * 2 + 1
                        }
                }
        }
}


# Plot the graph 
plot(t(x_train), t(y_train), type = "p", pch = 19, col = "blue",
     ylim = c(40, 100), xlim = c(min(x_train), max(x_train)),
     ylab = "Waiting time to next eruption (min)", xlab = "Eruption time (min)", las = 1, main = sprintf("P = %g", P))
points(t(x_test), t(y_test), type = "p", pch = 19, col = "red")

legend("topleft",
       c("training","test"),
       fill=c("blue","red"),
       bty="n")

lines(data_interval, y_predicted, type = "l", lwd = 2, col = "black")

#Calculating the RMSE 
RMSE <- sqrt(sum(sapply(1:length(data_interval), function(b) {sum(t(y_test[(data_interval[b] < x_test & x_test <= data_interval[b+1]),]- y_predicted[b])^2)}))/length(t(y_test)))
sprintf("RMSE is %g when P is %g", RMSE, P)

# Applying the same procedures for each P values given in the homework description. 
P=seq(from =5, to = 50, by = 5)
RMSE=rep(0,10)
for (t in 1:10) {
        N_train <-as.numeric( length(t(y_train)))
        N_test <- as.numeric(length(t(y_test)))
        
        node_indices <- list()
        is_terminal <- c()
        need_split <- c()
        
        node_features <- c()
        node_splits <- c()
        node_frequencies <- list()
        
        node_indices <- list(1:N_train)
        is_terminal <- c(FALSE)
        need_split <- c(TRUE)
        
        
        while (1) {
                # find nodes that need splitting
                split_nodes <- which(need_split)
                # check whether we reach all terminal nodes
                if (length(split_nodes)==0) {
                        break
                }
                # find best split positions for all nodes
                for (split_node in split_nodes) {
                        data_indices <- node_indices[[split_node]]
                        need_split[split_node] <- FALSE
                        node_frequencies[[split_node]] <- length(data_indices) 
                        
                        # check whether node is pure
                        if (length(data_indices)<=(P[t])) {
                                is_terminal[split_node] <- TRUE
                        } else {
                                is_terminal[split_node] <- FALSE
                                
                                
                                x_train_sort <- t(sort(t(unique(x_train[data_indices,]))))
                                best_scores <- rep(0, length(x_train_sort))
                                best_splits <- rep(0,  length(x_train_sort))
                                split_positions <- (x_train_sort[-1] + x_train_sort[-length(x_train_sort)]) / 2
                                split_scores <- rep(0, length(split_positions))
                                for (s in 1:length(split_positions))  {
                                        
                                        left_indices <- data_indices[(x_train[data_indices,] < split_positions[s])]
                                        right_indices <- data_indices[(x_train[data_indices,] >= split_positions[s])]
                                        mean_left=sum(y_train[left_indices,])/length(left_indices)
                                        mean_right=sum(y_train[right_indices,])/length(right_indices)
                                        split_scores[s] <- (sum((as.vector(y_train[left_indices,])-mean_left)^2) + sum((as.vector(y_train[right_indices,])-mean_right)^2))/ length(data_indices)
                                }
                                best_scores <- min(split_scores[1:length(split_positions)])
                                best_splits<- split_positions[which.min(split_scores[1:length(split_positions)])]
                                
                                # decide where to split on which feature
                                split_d <- which.min(split_scores[1:length(split_positions)])
                                node_features[split_node] <- split_d
                                node_splits[split_node] <- best_splits
                                
                                # create left node using the selected split
                                left_indices <- data_indices[(x_train[data_indices,] < split_positions[split_d])]
                                node_indices[[2 * split_node]] <- left_indices
                                is_terminal[2 * split_node] <- FALSE
                                need_split[2 * split_node] <- TRUE
                                
                                # create left node using the selected split
                                right_indices <- data_indices[(x_train[data_indices,] >= split_positions[split_d])]
                                node_indices[[2 * split_node + 1]] <- right_indices
                                is_terminal[2 * split_node + 1] <- FALSE
                                need_split[2 * split_node + 1] <- TRUE
                        }
                }
        }
        # extract rules
        terminal_nodes <- which(is_terminal)
        for (terminal_node in terminal_nodes) {
                index <- terminal_node
                rules <- c()
                while (index > 1) {
                        parent <- floor(index / 2)
                        if (index %% 2 == 0) {
                                # if node is left child of its parent
                                rules <- c(sprintf("x%d < %g", node_features[parent], node_splits[parent]), rules)
                        } else {
                                # if node is right child of its parent
                                rules <- c(sprintf("x%d >= %g", node_features[parent], node_splits[parent]), rules)
                        }
                        index <- parent
                }
                print(sprintf("{%s} => [%s]", paste0(rules, collapse = " AND "), paste0(node_frequencies[[terminal_node]], collapse = "-")))
        }
        
        # traverse tree for test data points
        data_interval <- seq(from =1.5, to = 6, by = 0.01)
        y_predicted <- rep(0, length(data_interval))
        for (i in 1:length(data_interval)) {
                index <- 1
                while (1) {
                        if (is_terminal[index] == TRUE) {
                                y_predicted[i] <- sum(as.vector(y_train[node_indices[[index]],]))/length(t(y_train[node_indices[[index]],]))
                                break
                        } else {
                                if (data_interval[i] < node_splits[index])  {
                                        index <- index * 2
                                } else {
                                        index <- index * 2 + 1
                                }
                        }
                }
       
                
        
       
        }
        RMSE[t] <- sqrt(sum(sapply(1:length(data_interval), function(b) {sum(t(y_test[(data_interval[b] < x_test & x_test <= data_interval[b+1]),]- y_predicted[b])^2)}))/length(t(y_test)))
        sprintf("RMSE is %g when P is %g", RMSE, 5*t)
}
# Ploting the all RMSE values for various P values
plot(1:(10)*5, RMSE,
     type = "b", pch=19, cex=1 ,lwd = 2, las = 1,ylim = c(6, 8.5),
     xlab = "Pre-pruning size (P)", ylab = "RMSE")



