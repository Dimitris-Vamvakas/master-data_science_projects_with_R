# Topic 2
install.packages("readxl")
library(readxl)

# Load the dataset
SOIL_DATA_GR <- read_excel(choose.files())
head(SOIL_DATA_GR)
str(SOIL_DATA_GR) 
#a

#  i) Remove the 1st column
SOIL_DATA_GR <- SOIL_DATA_GR[-1] 
head(SOIL_DATA_GR)

# ii) Count the number of records with NA values in the data
sum(is.na(SOIL_DATA_GR))

#iii) Omit the records with NA values from the data
SOIL_DATA_GR <-na.omit(SOIL_DATA_GR)

sum(is.na(SOIL_DATA_GR))

#  iv) Scale the data

df<-scale(SOIL_DATA_GR)
head(df)
str(df)
summary(df)

nrow(df)  # ommited rows

# Calculate and round requested statistics
max_pH <- round(max(df[, "pH"]), 3);max_pH
min_Sand <- round(min(df[, "Sand %"]), 3);min_Sand
median_Clay <- round(median(df[, "Clay %"]), 3);median_Clay

# Print the calculated values
cat("Number of rows omitted due to NAs:", number_of_NA_rows, "\n")
cat("Maximum pH value (scaled):", max_pH, "\n")
cat("Minimum Sand % value (scaled):", min_Sand, "\n")
cat("Median Clay % value (scaled):", median_Clay, "\n")

#b

library(cluster)
# Decide the maximum number of possible clusters to check
n_clusters <- 6


# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)
# Initialize silhouette scores: silscores
silscores <- numeric(n_clusters)

# Initialize silhouette scores: silscores
silscores <- numeric(n_clusters)


# Look over 1 to n_clusters possible clusters
for (i in 1:n_clusters) {
  # Set the seed
  set.seed(123)
  # Fit the model: km.out
  km.out <- kmeans(df, centers = i)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
  # Save the silhoutte score
  silscores[i] <- summary(silhouette(km.out$cluster, dist(df)))["avg.width"]
}

# Simple plot of the wss: 
plot(wss, xlab = "Number of clusters", ylab = "WSS", type = "b", pch = 19, main = "Scree Plot (WSS)")

#c
# Plot Silhouettes
sls <- as.numeric(unlist(silscores))
plot(sls, xlab = "Number of clusters", ylab = "Silhouette", type = "b", pch = 19, main = "Scree Plot (Silhouette)")


#d
# Load libraries
library(factoextra)
library(ggplot2)

# Visualize clusters with k=3
set.seed(123)
cl <- kmeans(df, centers=3)
fviz_cluster(cl, data = df,
             palette = c("red", "green", "blue"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)

# Print the requested values
print("Centroids: ")
print(cl$centers)
print(round(cl$centers[1,4],3))
print(round(cl$centers[2,11],3))
cat("Row 100 belongs to cluster: ", cl$cluster[100], "; Row 101 belongs to cluster: ", cl$cluster[101], "\n")

#e
# Select only the first 4 attributes of the SOIL_DATA dataset
soil = SOIL_DATA_GR[2:5]

# Remove rows with NA values and scale the data
soil.clean <- na.omit(soil)
soil.scaled <- scale(soil.clean)

# Visualize clusters with k=3
set.seed(123)
cl <- kmeans(soil.scaled, centers=3)
fviz_cluster(cl, data = soil.scaled,
             palette = c("red", "green", "blue"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)
