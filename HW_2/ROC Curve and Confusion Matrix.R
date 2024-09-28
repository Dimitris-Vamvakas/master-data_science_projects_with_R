# ROC Curve and Confusion Matrix

install.packages('caret')
library(caret)

#b
install.packages('ROCR')
library(ROCR)
install.packages("ggplot2")
library(ggplot2)
TPR <- c(0.0,0.2,0.4,0.4,0.6,0.6,1.0,1.0)
FPR <- c(0.0,0.0,0.0,0.2,0.2,0.6,0.6,1.0)
roc_data <- data.frame(FPR,TPR)
plot(roc_data, type = "l", col = "blue", xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)", main = "ROC Curve")
points(FPR,TPR, pch = 19)
abline(0, 1, lty = 2)  # Adds a dashed line with slope = 1

#c.Select a threshold of 0.65 for classification and  construct the confusion matrix 
install.packages('caret')
library(caret)

# Create the data frame
data <- data.frame(
  PredictedProbability = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.54, 0.53, 0.52, 0.51, 0.5),
  ActualClass = c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0)
)

# Convert PredictedProbability to a binary class with a threshold
data$PredictedClass <- ifelse(data$PredictedProbability > 0.65, 1, 0)

# Convert ActualClass and PredictedClass to factors with specified levels
data$ActualClass <- factor(data$ActualClass, levels = c(1, 0))
data$PredictedClass <- factor(data$PredictedClass, levels = c(1, 0))

# Create and print the confusion matrix
cm <- confusionMatrix(data$PredictedClass, data$ActualClass)
print(cm)
