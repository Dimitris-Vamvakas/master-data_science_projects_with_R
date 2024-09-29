#Decision Tree

#b
library(readr)
library(dplyr)
x<-read.csv(choose.files(),header=T,sep = ",")
summary(x)
str(x)

eye_attr_entropy_calc <- function(x, attr, subsetParam) {
  subs <- subset(x, x[[attr]] == subsetParam)
  s <- nrow(subs)
  s1 <- sum(subs$Class == "A")  
  s2 <- sum(subs$Class == "B")  
  
  y <- c(s1/s, s2/s)
  
  if (s1 == 0 || s2 == 0)
    return(0)
  else
    return(-sum(y * log2(y)))
}


print( round( eye_attr_entropy_calc(x, "Age", "Middle-aged"), 3 ) )
print( round( eye_attr_entropy_calc(x, "Vision", "Myopia"), 3 ) )
print( round( eye_attr_entropy_calc(x, "Astigmatism", "Yes"), 3 ) )
print( round( eye_attr_entropy_calc(x, "UseOfGlasses", "Rare"), 3 ) )

#c
s <- nrow(x)
s1 <- nrow( subset(x,x$Class=='A') )
s2 <- nrow( subset(x,x$Class=='B') )
entropy_bef <- -sum(c(s1/s, s2/s)*log2(c(s1/s, s2/s))) 
for( i in colnames(x[,1:4]) ){
  entr <-0
  for( j in c(unique(x[,i])) ){
    val <- eye_attr_entropy_calc(x, i, j)
    ss <- nrow(subset(x,x[i]==j))
    entr <- entr + ss/s*val
  }
  inf_gain <- entropy_bef- entr
  cat('information gain of ', i, ' = ', round(inf_gain,3), '\n')
}

#d
library(caret)
library(rpart.plot)
eye_data <-read.csv(choose.files(), stringsAsFactors = TRUE)
colnames(eye_data)[5] <- "output"

#model
dt <-  rpart(output ~ Vision + Age + Astigmatism + UseOfGlasses, 
             method="class", 
             data=eye_data, 
             control=rpart.control(minsplit=1), 
             parms=list(split='information'))

rpart.plot(dt, type=4, extra=1) #plot decision tree

newdata1 <- data.frame(Age='Young',Vision='Myopia',Astigmatism='No',UseOfGlasses='Often')
newdata2 <- data.frame(Age='Elderly',Vision='Myopia',Astigmatism='Yes',UseOfGlasses='Often')

predict(dt,newdata=newdata1,type="class")
predict(dt,newdata=newdata2,type="class")

