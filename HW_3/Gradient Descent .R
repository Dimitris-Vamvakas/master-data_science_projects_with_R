#Gradient Descent

#a
# Install the AppliedPredictiveModeling package
install.packages("AppliedPredictiveModeling")
# Load the AppliedPredictiveModeling package
library(AppliedPredictiveModeling)

# Setting a seed for reproducibility is good practice, but here it is commented out
#set.seed(1)

# Load the abalone dataset
data(abalone)

# Get the number of rows (observations) in the abalone dataset
n <- nrow(abalone)

# Prepare the feature matrix X by binding a column of ones (for intercept) and the features from abalone dataset
# This ignores the first column (assumed to be the response variable) and uses columns 2 to 8 as features
X <- cbind(rep(1, n), data.matrix(abalone[,2:8]))
# Remove column names for X
colnames(X)<-NULL
# Set y as the response variable, which is the first column of the abalone dataset
y <- abalone[,9]
# Initialize theta (parameters) as a vector of zeros with length equal to the number of features plus one (for intercept)
theta <- rep(0,8)
theta

#b
# Define the cost function to compute the Mean Squared Error (MSE) between the predicted and actual values
mse_cost <- function(X, y, theta){
  
  # Calculate the number of observations
  n <- length(y)
  
  # Compute the sum of squared differences, divided by 2*n, between predicted and actual values
  s <- sum( (X%*%theta - y)^2 )/(2*n)
  return(s)
  
}

# Test the cost function with a simple example
X0 <- cbind(rep(1,10), rep(1,10) )
y0 <- 5*rep(1,10)
theta0 = rep(1, 2)

# Calculate MSE for the test case
mse_cost(X0, y0, theta0)

#c
# Define the gradient descent function to minimize the MSE cost
gradientDescent <- function(X, y, theta, learning_rate, num_iter){
  
  # Number of observations
  nrOfObj <- length(y)
  # Initialize a vector to store the cost at each iteration
  l_loss <- rep(0, num_iter)
  
  # Perform gradient descent for a specified number of iterations
  for( i in 1:num_iter){
    
    # Calculate the gradient of the cost function
    grad <- (1/nrOfObj) * ( t(X) %*% ( X %*%theta - y  ) )
    
    # Update theta by subtracting a fraction (defined by the learning rate) of the gradient
    theta <- theta - learning_rate*grad  
    # Store the cost in l_loss vector for each iteration
    l_loss[i] <- mse_cost(X, y, theta)
  }
  # Return the final theta and loss history as a list
  res <-list(theta, l_loss)
  return(res)
}

#d
# Set the learning rate for gradient descent
alpha <- .1

# Set the number of iterations for gradient descent
num_iters <- 150

# Run gradient descent on the abalone dataset with specified parameters
results <- gradientDescent(X, y, theta, alpha, num_iters)

# Extract the final theta and loss history from the results
theta_final <- results[[1]]
loss_history <- results[[2]]
# Print the final theta values, rounded to 3 decimal places
print( round( theta_final, 3 ) )
# Plot the loss history over iterations
plot(1:num_iters, loss_history)
