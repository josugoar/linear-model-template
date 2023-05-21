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
    scatter.smooth(x=data[[col_name]], y=data$Concrete.compressive.strength, main=col_name, col="lightgreen")
  }
}

# Correlation between variables
print("Correlation between each attribute and Concrete.compressive.strength: A low correlation (-0.2 < x < 0.2)", quote=FALSE)

for (col_name in colnames(data)) {
  print(paste0(col_name, ": ", cor(data$Concrete.compressive.strength, data[[col_name]])), quote=FALSE)
}

for (col_name in colnames(data)) {
  correlation <- round(cor(data$Concrete.compressive.strength, data[[col_name]]), 2)
  label = paste0(col_name, ": ", correlation)
  print(ggplot(data, aes(y=Concrete.compressive.strength, x=.data[[col_name]])) + geom_point() +
          geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
          labs(title = label, y = "Concrete.compressive.strength", x = col_name))
}

# Percentage of training examples
training_p <- 0.7

best_model <- NULL
best_mean_avg_error <- Inf

for (i in 1:10) {
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
  
  if (mean_avg_error < best_mean_avg_error) {
    best_model <- model
    best_mean_avg_error <- mean_avg_error
  }
}

# Print Mean Absolute Error
print(paste0("Mean average error: ", best_mean_avg_error))

# Print model summary
summary(best_model)

# Calculate Absolute Error
abs_error <- abs(predict(model, data) - data$Concrete.compressive.strength)

# Print sample with Max Absolute Error
print(paste0("Sample ", which.max(abs_error), " with max absolute error ", max(abs_error)))

print("Given any sample, how many units of water must be added or subtracted for the resistance to increase by 10 points?")
difference <- mean(((data$Concrete.compressive.strength + 10 -
                     coef(best_model)["(Intercept)"] -
                     coef(best_model)["Cement"] * data$Cement -
                     coef(best_model)["Blast.Furnace.Slag"] * data$Blast.Furnace.Slag -
                     coef(best_model)["Fly.Ash"] * data$Fly.Ash -
                     coef(best_model)["Superplasticizer"] * data$Superplasticizer -
                     coef(best_model)["Coarse.Aggregate"] * data$Coarse.Aggregate -
                     coef(best_model)["Fine.Aggregate"] * data$Fine.Aggregate -
                     coef(best_model)["Age"] * data$Age) /
                    coef(best_model)["Water"]) -
                   data$Water)
print(paste0(difference, " units of water"))

print("Which sample will reduce its strength the most if the superplasticizer is removed?")
differences <- sort(data$Concrete.compressive.strength -
                    (coef(best_model)["(Intercept)"] +
                     coef(best_model)["Cement"] * data$Cement +
                     coef(best_model)["Blast.Furnace.Slag"] * data$Blast.Furnace.Slag +
                     coef(best_model)["Fly.Ash"] * data$Fly.Ash +
                     coef(best_model)["Water"] * data$Water +
                     coef(best_model)["Coarse.Aggregate"] * data$Coarse.Aggregate +
                     coef(best_model)["Fine.Aggregate"] * data$Fine.Aggregate +
                     coef(best_model)["Age"] * data$Age),
                    decreasing = TRUE,
                    index.return = TRUE)
print(paste0("Sample ", differences$ix[1], " by ", differences$x[1], " points"))

print("Which are the 3 samples that increase their resistance the most by adding 5 units of superplasticizer?")
differences <- sort((coef(best_model)["(Intercept)"] +
                     coef(best_model)["Cement"] * data$Cement +
                     coef(best_model)["Blast.Furnace.Slag"] * data$Blast.Furnace.Slag +
                     coef(best_model)["Fly.Ash"] * data$Fly.Ash +
                     coef(best_model)["Water"] * data$Water +
                     coef(best_model)["Superplasticizer"] * data$Superplasticizer + 5 +
                     coef(best_model)["Coarse.Aggregate"] * data$Coarse.Aggregate +
                     coef(best_model)["Fine.Aggregate"] * data$Fine.Aggregate +
                     coef(best_model)["Age"] * data$Age) -
                      data$Concrete.compressive.strength,
                    decreasing = TRUE,
                    index.return = TRUE)

for (i in 1:3) {
  print(paste0("Sample ", differences$ix[i], " with difference ", differences$x[i], " points"))
}
