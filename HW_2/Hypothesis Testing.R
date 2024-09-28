#Hypothesis Testing

#a. Create a dataframe including the data from the productivity_data.csv file. Define two 
#hypotheses (null and alternative) in the fields below to (i) check whether the training 
#did not change productivity and (ii) check whether the training did not improve productivity

productivity_data<-file.choose()
dt<- read.csv(productivity_data, sep=";")
head(dt)

#b
#Ho (null Hypothesis):  The training did not change productivity 
#H1 (alternative Hypothesis):  The training changed productivity

#t-test results to check change in productivity
t_test_result <- t.test(dt$Before, dt$After, paired = TRUE)
cat("p value to check change in productivity=", round(t_test_result$p.value, 3), "\n")
#Since the p-value is less than the significance level of 5%, we can reject the 
#null hypothesis - there is a statistically significant change in productivity 

#Ho (null Hypothesis):  The training did not improve productivity 
#H1 (alternative Hypothesis):  The training improved productivity

# Performing a one-sided paired t-test
# Testing if the mean productivity after training is greater than before training
t_test_result1 <- t.test(dt$Before, dt$After, paired = TRUE, alternative = "less")
cat("p value to check improvement in productivity=", round(t_test_result1$p.value, 3), "\n")
#Since the p-value is less than the significance level of 5%, we can reject the 
#null hypothesis - there is a statistically significant improvement in productivity


#e. The company conducted a survey to measure the employee’s satisfaction of this training program

#Ho (null Hypothesis):  There is no relationship between gender and the level 
#of satisfaction 
#H1 (alternative Hypothesis):  There is a relationship between gender and the level of 
#satisfaction 

# Performing a Chi-Squared Test
data<-matrix(c(35,41,23,16),nr=2)
rownames(data)<-c("Male","Female")
colnames(data)<-c("Satisfied","Unsatisfied")
data

#significance level of a=0.05
chisq_test<-chisq.test(data,correct=FALSE)
cat("p value for chi-squared test=", round(chisq_test$p.value, 3), "\n")

# Since the p-value is greater than the significance level of 5%, we cannot reject the null hypothesis. 

