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
library(RKEEL)

# Read data from CSV
data <- read.keel("../data/baseball.dat")
# Convert string to integer
data <- transform(data, Salary = strtoi(Salary))
# Remove non-numerical columns of the data
data$Free_agency_eligibility = NULL
data$Free_agent = NULL
data$Arbitration_eligibility = NULL
data$Arbitration = NULL

# Set plot structure and margins
par(mfrow = c(2,6), mar=c(1,1,1,1))
# Scatter Plot - Check linear relationships
for (col_name in colnames(data)) {
  if (col_name != "Salary") {
    scatter.smooth(x=data$Salary, y=data[[col_name]], main=col_name, col="lightgreen")
  }
}

# Correlation between variables
print("Correlation between each attribute and Salary: A low correlation (-0.2 < x < 0.2)", quote=FALSE)

for (col_name in colnames(data)) {
  print(paste0(col_name, ": ", cor(data$Salary, data[[col_name]])), quote=FALSE)
}

# Percentage of training examples
training_p <- 0.8

# Generate data partition 80% training / 20% test. The result is a vector with 
# the indexes of the examples that will be used for the training of the model.
training_indexes <- createDataPartition(y = data$Salary, p = training_p, list = FALSE)

# Split training and test data
training_data <- data[training_indexes, ]  # Extract training data using training_indexes
test_data     <- data[-training_indexes, ] # Extract data with the indexes not included in training_indexes 

# Create Linear Model using training data. Formula = all the columns except Salary
model <- lm(formula = Salary ~., data = training_data)

# Make the prediction using the model and test data
prediction <- predict(model, test_data)

# Calculate Mean Average Error
mean_avg_error <- mean(abs(prediction - test_data$Salary))
  
# Print Mean Absolute Error
print(paste0("- Mean average error: ", mean_avg_error))

# Print model summary
summary(model)