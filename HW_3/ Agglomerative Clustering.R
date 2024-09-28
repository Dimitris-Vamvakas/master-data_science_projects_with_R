#Topic 3

#a
# report the mean and standard deviation 
#values of the attributes of the original and the scaled data.
install.packages("cluster")
library("cluster")
data("USArrests")
df<- as.data.frame(scale(USArrests,center=T,scale= T))
round(sapply(df,mean),3)
round(sapply(df,sd),3)
round(sapply(USArrests,mean),3)
round(sapply(USArrests,sd),3)

#b
# define a function that takes a method x and computes the agglomerative coefficient
# by the agnes function on the scaled data
agglom_coef <- function(x){
  agnes(df,method=x)$ac
}
linkage_methods <- c("single","average","complete")
round(sapply(linkage_methods,agglom_coef), 3 )

#c
# calculate the Euclidean distance matrix-Plot the 
#dendrogram of the clustering
d_eucl <- dist(df,method = "euclidean")

clust_complete <- hclust(d_eucl,method="complete")

plot(clust_complete)

#d
# Cut the dendrogram to create 5 clusters and assign each observation to a cluster
ind <- cutree(clust_complete, k=5)
# Combine the original data with the cluster assignments
data_5_clusters <- cbind(data, clst = ind)

# Display the first few rows of the data with cluster assignments
head(data_5_clusters)

# Calculate and round the mean of the original data grouped by cluster assignment, for each variable, to 3 decimal places
round( aggregate(data_5_clusters, by=list(data_5_clusters$clst), mean), 3 )


