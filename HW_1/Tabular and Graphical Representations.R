#Tabular and Graphical Representations

install.packages("carData")
library(carData)
salaries_data<-Salaries

#a. Inspect the dataset and print its first six rows
str(Salaries)
# 6 variables(=attributes)(rank,discipline,yrs.since.phd,yrs.service,sex,salary)
# 397 instances(rows)
#data_type: factors-> rank (ordinal,3 levels="AsstProf","AssocProf","Prof"),discipline(ordinal,2 levels="A","B"),sex(categorical,2 levels="Female","Male")
#int: yrs.since.phd(numerical,discrete,ratio),yrs.service(numerical,discrete,ratio),salary(numerical,continuous,ratio)
head(Salaries)

#b. Create a contingency table, with absolute frequencies for the attributes rank and sex.
print(addmargins(table(salaries_data$rank, salaries_data$sex, dnn = c("Rank", "Sex"))))

#c. Create a mosaic plot for the contingency table of the previous question. Do not include 
#the totals of each column/row and use different colours in order to make it easy to 
#identify attribute values. 
mosaicplot(table(salaries_data$rank, salaries_data$sex), color = c("red", "green"), xlab = "Rank", ylab = "Sex")

#d.Create a histogram with absolute frequencies for the attribute salary using at least seven bins.
hist(Salaries$salary, breaks=7)
