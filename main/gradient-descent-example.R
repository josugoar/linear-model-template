# Remove all the variables in the workspace
rm(list = ls())
# Clear console
cat("\014")
# Clear plots
if(!is.null(dev.list())) dev.off()

# Load ggplot2 library
library(ggplot2)

# Sets working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define the training set (for 2 degrees: 1 variable and x0)
training_set    <- data.frame(x1=seq(1:20))
training_set$x0 <- 1 # this is the intercept
training_set$y  <- 2 * training_set$x1 - 3 + 4 * runif(nrow(training_set), 0, 1) - 2

# Define weights for the learning hypothesis and alpha
w0 <- 0
w1 <- 0
alpha <- 0.01

# Calculate the hypothesis using current weights
training_set$h <- training_set$x1 * w1 + training_set$x0 * w0
# Calculate the current error
training_set$e <- training_set$y - training_set$h

# Show training set in the command line
training_set

# Plot data and hypothesis before applying Linear Regression
ggplot(training_set) + 
  geom_point(aes(x=x1, y=y, col="Data")) + 
  geom_point(aes(x=x1, y=h, col="Hypothesis")) + 
  labs(title=paste0("Initial Situation"), caption=paste0("w1=", round(w1, digits=3),"  w0=", round(w0, digits=3)))

# Update weights
w1 <- w1 + alpha * sum(training_set$e * training_set$x1) / nrow(training_set)
w0 <- w0 + alpha * sum(training_set$e * training_set$x0) / nrow(training_set)

# Calculate again the hypothesis and the error after updating the weights one time
training_set$h <- training_set$x1 * w1 + training_set$x0 * w0
training_set$e <- training_set$y - training_set$h

# Show training set in the command line
training_set

# Plot data and hypothesis after updating the weights one time
ggplot(training_set) + 
  geom_point(aes(x=x1, y=y, col="Data")) + 
  geom_point(aes(x=x1, y=h, col="Hypothesis")) + 
  labs(title=paste0("After 1 update"), caption=paste0("w1=", round(w1, digits=3), "  w0=", round(w0, digits=3)))

# Set the number of iterations for the learning process
iterations <- 1000

for (i in 1:iterations) {
  # Update weights
  w1 <- w1 + alpha * sum(training_set$e * training_set$x1) / nrow(training_set)
  w0 <- w0 + alpha * sum(training_set$e * training_set$x0) / nrow(training_set)
  # Update the hypothesis value and the error
  training_set$h <- training_set$x1 * w1 + training_set$x0 * w0
  training_set$e <- training_set$y - training_set$h
  # Print the evolution of the weigths and error during the learnig process
  print(paste0(i, "  w1=", round(w1, digits=3),
               "  w0=", round(w0, digits=3),
               "  error=", round(mean(abs(training_set$e)), digits=3)), quote = FALSE)
}

# Plot data and hypothesis after finishing the learning process
ggplot(training_set) + 
  geom_point(aes(x=x1, y=y, col="Data")) + 
  geom_point(aes(x=x1, y=h, col="Hypothesis")) + 
  labs(title=paste0("Iteration ", i), caption=paste0("w1=", round(w1, digits=3),"  w0=", round(w0, digits=3)))
