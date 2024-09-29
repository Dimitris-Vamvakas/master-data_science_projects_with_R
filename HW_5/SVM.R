#SVM

#a
library(dplyr)
data("iris")
str(iris)
iris <- iris %>% mutate(Class = case_when(Species =='setosa' ~ 1,Species!= 'setosa' ~ -1))
ds <- iris[,c(3,4,6)]
w = c(1,1)
C <- 1
r <- 0.1

calc_J <- function (ds, w, C){
  #calculate the max
  tmp <- 1 - (w[1]*ds$Petal.Length+w[2]*ds$Petal.Width)*ds$Class
  mMax <- pmax(tmp, 0, na.rm = TRUE)
  return(((1/2)*sum(w^2))+C*sum(mMax))
}
w[1]=0.1
w[2]=0.1
C <- 0.5
calc_J(ds, w, C)

w[1]=0.1
w[2]=0.1
C=1
calc_J(ds, w, C)

w[1]=0.5
w[2]=0.5
C=1
calc_J(ds, w, C)

w[1]=1
w[2]=1
C=0.5
calc_J(ds, w, C)

#b
calc_DJ <- function (ds, w, C){
  #calculate the max
  tmp <- 1-(w[1]*ds$Petal.Length+w[2]*ds$Petal.Width)*ds$Class
  mMax <- pmax(tmp, 0, na.rm = TRUE)
  #find values which are 0
  indices <-t(which(mMax!=0, arr.ind = TRUE))
  d1 <- (w[1] -(1/150)* (C * sum(ds$Petal.Length[indices] * ds$Class[indices])))
  d2 <- (w[2] -(1/150)* (C * sum(ds$Petal.Width[indices] * ds$Class[indices])))
  return (c(d1,d2))
}

w[1]=0.2
w[2]=0.2
C=0.5
calc_DJ(ds, w, C)

w[1]=0.5
w[2]=0.5
C=1
calc_DJ(ds, w, C)

w[1]=1
w[2]=1
C=0.5
calc_DJ(ds, w, C)

#c
head(ds,5)

w[1]=1.4
w[2]=0.2
C <- 1
calc_J(ds, w, C)
calc_DJ(ds, w, C)

w[1]=1.4
w[2]=0.2
C <- 1
calc_J(ds, w, C)
calc_DJ(ds, w, C)

#d
w = c(1,1) # Initialize vector w, so as to avoid continuing from question 3c.
set.seed(2024)
for( i in 1:5 ){
  indices <- round(runif(50, min=1, max=nrow(ds)),0)
  J <- calc_J (ds[indices,], w, C)
  DJ <- calc_DJ (ds[indices,], w, C)
  w <- w - r * DJ
  cat('iteration = ', i, '\n')
  cat('J = ', round(J,3), '\n')
  cat('dJ = ', round(DJ,3), '\n')
  cat('w = ', round(w,3), '\n\n')
}

