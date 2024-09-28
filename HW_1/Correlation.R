#Correlation 

install.packages("carData")
library(carData)
states_data<-States
head(states_data)

install.packages('ggplot2')
library(ggplot2)

#a. Draw a scatter plot to visually check if there exists a relationship between SATV (x-axis) and dollars (y-axis). 
plot(states_data$SATV, states_data$dollars, main="SATV/dollars", pch=16, cex=1, xlab="SATV", ylab="dollars", col="red")
#SATV and dollars have negative correlation because when SATV increases, dollars decreases and vice versa.

#b.  identify which one of the following pairs of variables has the greatest correlation (either positive or negative)
par(mfrow = c(2, 2))
plot(states_data$SATV, states_data$pay, main="SATV/pay", pch=16, cex=0.5, xlab="SATV", ylab="dollars", col="red")
plot(states_data$SATV, states_data$pop, main="SATV/pop", pch=16, cex=0.5, xlab="SATV", ylab="dollars", col="red")
plot(states_data$SATM, states_data$dollars, main="SATM/dollars", pch=16, cex=0.5, xlab="SATV", ylab="dollars", col="red")
plot(states_data$SATM, states_data$pop, main="SATM/pop", pch=16, cex=0.5, xlab="SATV", ylab="dollars", col="red")
#From the above plots, we can claim that the pair SATV and pay has the strongest correlation (negative).  

#c. Verify your answer in the previous question by calculating in R the value of the 
#Spearman’s rank correlation coefficient for each of the pairs under consideration.
round(cor(states_data$SATV, states_data$pay, method="spearman"), 3) #-0.564
round(cor(states_data$SATV, states_data$pop, method="spearman"), 3) #-0.304
round(cor(states_data$SATM, states_data$dollars, method="spearman"), 3)#-0.47
round(cor(states_data$SATM, states_data$pop, method="spearman"), 3) #-0.205
#The abs of correlation between SATV and pay is the biggest, so it's the strongest.This is not recognisable from visualization.
