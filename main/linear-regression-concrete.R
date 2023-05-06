# Clear Environment
rm(list=ls())
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Clear plots & console
if(!is.null(dev.list())) dev.off()
cat("\014") 

# Install required packages
library(ggplot2)
library(lattice)
library(caret)

# Read data
filename = "../data/concrete.csv"
data <- read.csv(file=filename, sep=";", header = TRUE)

par(mfrow = c(2,4), mar=c(1,1,1,1))

# Scatter Plot - Check linear relationships
for (col_name in colnames(data)) {
  if (col_name != "Concrete.compressive.strength") {
    scatter.smooth(x=data$Concrete.compressive.strength, y=data[[col_name]], main=col_name, col="lightgreen")
  }
}

# Correlation between variables
print("Correlation between each attribute and Concrete.compressive.strength: A low correlation (-0.2 < x < 0.2)", quote=FALSE)

for (col_name in colnames(data)) {
  print(paste0(col_name, ": ", cor(data$Concrete.compressive.strength, data[[col_name]])), quote=FALSE)
}

# Percentage of training examples
training_p <- 0.7

# Generate data partition 70% training / 30% test. The result is a vector with 
# the indexes of the examples that will be used for the training of the model.
training_samples <- createDataPartition(y = data$Concrete.compressive.strength, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_samples, ]
test_data     <- data[-training_samples, ]

# Create Linear Model using training data. Formula = all the columns except Concrete.compressive.strength
model <- lm(formula = training_data$Concrete.compressive.strength ~., data = training_data)

# Make the prediction using the model and test data
prediction <- predict(model, test_data)

# Calculate Mean Average Error
mean_avg_error <- mean(abs(prediction - test_data$Concrete.compressive.strength))

# Print Mean Absolute Error
print(paste0("- Mean average error: ", mean_avg_error))

# Print model summary
summary(model)