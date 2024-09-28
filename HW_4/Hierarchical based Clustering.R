#Hierarchical based Clustering

#a
# Load and inspect data
fruits_data <- read.csv2(choose.files())
# View(fruits_data)

str(fruits_data)
row.names(fruits_data) <- fruits_data$Name
fruits_data <- fruits_data[,-1]

# Scale the data
fruits_data.scaled <- scale(fruits_data)
fruits_data.scaled <- as.data.frame(fruits_data.scaled)
str(fruits_data.scaled)

# Calculate first metrics
median_sugar = round(median(fruits_data.scaled$Sugars_g), 3)
max_energy = round(max(fruits_data.scaled$Energy_kcal), 3)
min_water = round(min(fruits_data.scaled$Water_g), 3)

cat("The median value of the Sugars_g attribute is: ", median_sugar, "\n")
cat("The maximum value of the Energy_kcal attribute is: ", max_energy, "\n")
cat("The minimum value of the Water_g attribute is: ", min_water, "\n")


#b
# Calculate the distance values
fruits.dist.eucl <- dist(as.matrix(fruits_data.scaled), method="euclidean")
orange_apple <- round(as.matrix(fruits.dist.eucl)["Orange","Apple"],3)
banana_peach <- round(as.matrix(fruits.dist.eucl)["Banana","Peach"],3)
lemon_mango <- round(as.matrix(fruits.dist.eucl)["Lemon","Mango"],3)

cat("The Euclidean distance between Apple and Orange is: ", orange_apple, "\n")
cat("The Euclidean distance between Banana and Peach is: ", banana_peach, "\n")
cat("The Euclidean distance between Lemon and Mango is: ", lemon_mango, "\n")


# Find the fruit closest to Pear
pear_sorted <- sort(as.matrix(fruits.dist.eucl)["Pear",])

cat("The fruit closest to Pear is: ", names(pear_sorted[2]), " and the corresponding Euclidean distance is : ", round(pear_sorted[2],3), "\n")

#c
# Perform agglomerative hierarchical clustering 
fruits.hcl.complete <- hclust(fruits.dist.eucl, method="complete")
plot(as.dendrogram(fruits.hcl.complete))

fruits.hcl.single <- hclust(fruits.dist.eucl, method="single")
plot(as.dendrogram(fruits.hcl.single))

#d
# Find clusters when k=5
fruits.clusters <- as.matrix(cutree(fruits.hcl.complete, k=5))
fruits.clusters
for (i in 1:5) {
  cat("Cluster", i, ": ", rownames(fruits.clusters, do.NULL=True)[which(fruits.clusters[,1]==i)], "\n")
  
}

#e
# Check Orange
orange_val <- fruits.clusters["Orange", ]
cat("The fruits that belong to the same cluster as Orange are: ",rownames(fruits.clusters, do.NULL=True)[which(fruits.clusters[,1]==orange_val)])


### Check for Orange, Grapefruit, Nectarine, Lemon and Mandarine 
k_max <- 1
citrus = c("Orange", "Grapefruit", "Nectarine", "Lemon", "Mandarin")
max_classes = nrow(fruits_data.scaled)-length(citrus)

for (k_test in 1:max_classes) { 
  fruits.clusters <- as.matrix(cutree(fruits.hcl.single, k=k_test)) 
  key_val <- fruits.clusters[citrus[1], ]
  same_cluster <- TRUE  
  for (i in 2:length(citrus)) { 
    if (key_val != fruits.clusters[citrus[i], ]) { 
      same_cluster <- FALSE 
      break 
    } 
  } 
  if (same_cluster == FALSE) { 
    k_max <- k_test - 1 
    break 
  } 
}


cat("Maximum number of clusters is: ", k_max)


# Find and plot the height for this solution
h.single <- sort(fruits.hcl.single$height, decreasing=T)[k_max]
cat("The corresponding height is: ", round(h.single,3), "\n")
plot(as.dendrogram(fruits.hcl.single))
abline(col = 'red', h = h.single)
