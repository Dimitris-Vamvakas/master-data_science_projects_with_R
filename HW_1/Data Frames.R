#Data Frames
data("Salaries")
salaries_data<-Salaries
head(Salaries)

#a. Identify the number of professors (of all ranks) that work on Discipline A.
nrow(salaries_data[salaries_data$discipline == "A",])

#b. Identify the number of male and female professors (of all ranks) that have a salary of more than 150000$
nrow(salaries_data[salaries_data$sex == "Male" & salaries_data$salary > 150000,])
nrow(salaries_data[salaries_data$sex == "Female" & salaries_data$salary > 150000,])

#c.Calculate, the average years of service for professors of all ranks and the 
#average years of service for Associate Professors
x<-mean(Salaries$yrs.service)
round(x,digits = 3)
y<-mean(subset(salaries_data$yrs.service, salaries_data$rank == "AssocProf"))
round(y,digits=3)

#d.Create a new ordinal attribute called career_stage which may take the values early,  
#mid and late that correspond to the following intervals (-1, 10], (10, 25], and (25, 60], 
#respectively. Calculate the number of professors thar are at an early stage of their career. 
salaries_data$career_stage <- cut(salaries_data$yrs.service, breaks = c(-1, 10, 25, 60), labels = c("early", "mid", "late"))

nrow(salaries_data[salaries_data$career_stage == "early",])

#e. Print out the data of the 3 professors, which have the most years of service, after first 
#ordering the data frame in decreasing order based on the attribute yrs.service
ord <- order(salaries_data$yrs.service, decreasing = TRUE)
print(head(salaries_data[ord,], 3))
