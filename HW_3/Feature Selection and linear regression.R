#Feature Selection and linear regression

#a

# install and load AppliedPredictiveModeling
install.packages("AppliedPredictiveModeling")
install.packages("Metrics")
library(AppliedPredictiveModeling)
library(caret)
library(Metrics)
# load the abalone dataset
data(abalone)
# exclude the Type and Rings variables and save the dataframe in the df_data variable
library(readr)
library(dplyr)
df_data <- select(abalone,-Type,-Rings)
head(df_data)
# keep the Rings variable to y
y <- abalone$Rings
ranking <- filterVarImp(df_data,y,nonpara = FALSE)

# sort the variables by decreasing importance
ranking$varNames <- rownames(ranking)
ranked_vars <-arrange(ranking,desc(ranking$Overall))
ranked_vars$varNames
round(ranked_vars$Overall, 3)

#b
# Select the top 2 most important variables for modeling
x_rank <- df_data[,ranked_vars$varNames[1:2] ]
# Scale the selected variables for normalization
x_rank <- scale(x_rank)
# Fit a linear model using the selected and scaled variables against 'y'
model_rank <- lm(y~x_rank)

# Calculate and display the mean squared error of the model, rounded to 3 decimal places
round(mse(y, predict(model_rank)), 3)

#c
new_df <- as.data.frame(scale(df_data)) 
new_df.pca <- prcomp(new_df)
summary(new_df.pca)


#d
# Prepare data with the first two principal components for modeling
x_PCA <- new_df.pca$x
x_PCA
x_PCA2 <- as.data.frame(x_PCA[,c(1,2)])
x_PCA2
# Add the response variable 'y' to the PCA dataset for modeling
x_PCA2$age <- y
# Fit a linear model using the first two principal components to predict 'age'
model_PCA <- lm(age~PC1+PC2, data=x_PCA2)
# Calculate and display the mean squared error of the PCA-based model, rounded to 3 decimal places
round( mse(x_PCA2$age, predict(model_PCA)), 3)
