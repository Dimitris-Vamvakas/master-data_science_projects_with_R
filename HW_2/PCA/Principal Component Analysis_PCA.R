#Principal Component Analysis-PCA

file_path <- file.choose()
wine_data <- read.csv(file_path, sep=";")

#a. Report the type of magnesium and flavonoids attributes and print the first five rows of the dataset
str(wine_data)
class(wine_data$magnesium)
class(wine_data$flavonoids)
head(wine_data,n=5)

#b. After you exclude the target column from your data, using R, calculate the mean and 
#standard deviation of all attributes
wine_data_notarget <- subset(wine_data, select=-target)
head(wine_data_notarget)

means <- round(sapply(wine_data_notarget, mean, na.rm = TRUE),3)
std_devs <- round(sapply(wine_data_notarget, sd, na.rm = TRUE),3)

print(means)
print(std_devs)

#c.  Standardize the data and use a PCA technique to extract the principal components
wine.pca <- prcomp(wine_data_notarget,
                   center=T,scale=T)
print(wine.pca)
summary(wine.pca)
#36,2% of the variance is explained by the first principal component, 
#19,21% by the second principal component, 
#11,12% by the third principal component,   
#7,069% by the fourth principal component and 
#6,563% by the fifth principal component.

#d.  Create a scree plot
data_var<-wine.pca$sdev^2
pve= data_var/sum(data_var)
pve
plot(pve, xlab = "Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,0.5), 
     type="b")

