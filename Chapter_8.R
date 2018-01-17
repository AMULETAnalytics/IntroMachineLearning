# File-Name:       Chapter_8.R           
# Date:            2015-10-10                                
# Author:          Daniel D. Gutierrez (daniel@amuletanalytics.com)
# Purpose:         Machine Learning and Data Science: code for Chapter 8 - Unsupervised Learning

# All source code is copyright (c) 2015, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.


# -------------------------------------------------------
# Simulating clusters
# -------------------------------------------------------

set.seed(1234)  # Set seed to get same data set each time
par(mar=c(0,0,0,0))  # Set plot margins: c(bottom, left, top, right)

# Define numeric vector, length=12
# vector of means: 1 1 1 1 2 2 2 2 3 3 3 3
x <- rnorm(12, mean=rep(1:3, each=4), sd=0.2)

# Define numeric vector, length=12
# vector of means: 1 1 1 1 2 2 2 2 1 1 1 1
y <- rnorm(12, mean=rep(c(1,2,1), each=4), sd=0.2)

# The way we've created the x and y coordinates creates 3 clusters
plot(x,y, col="dark green", pch=19, cex=3)

# Display integer labels to the upper-right of the dot
text(x+0.05, y+0.05, labels=as.character(1:12))


# -------------------------------------------------------
# Hierarchical clustering with simulated data set
# -------------------------------------------------------

df <- data.frame(x=x, y=y)   # 12x2
# Calculate distance between points. dist() is just for clustering
dist(df)    # Calculate and display distance between variables

# Calculate distance between 12 points observed (distance between columns)
distxy <- dist(df)   # Default distance method = euclidean metric
#distxy <- dist(df, method="minkowski")   # Class=dist!!


# Produce cluster object
hClustering <- hclust(distxy, method="complete")  # hclust requires a dist object, returns hclust object
# Plot dendrogram showing 3 clusters
plot(hClustering)   

# Cut the tree high yields fewer clusters
cutree(hClustering,h=1.5)   # Will yield fewer clusters
# [1] 1 1 1 1 2 2 2 2 3 3 3 3

# Cut the tree low
cutree(hClustering,h=0.5)   # Will yield more clusters
# [1] 1 2 2 1 3 3 3 4 5 5 5 5


# -------------------------------------------------------
# Visualizing hierarchical clustering using a Heatmap
# -------------------------------------------------------

dataFrame <- data.frame(x=x, y=y)
set.seed(143)

# Take a small sample of the rows. Each sample() returns different random seq
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]  # 12x2

# Clusters together the rows and columns
heatmap(dataMatrix)   # heatmap() requires matrix argument


# -------------------------------------------------------
# Hierarchical clustering with iris data set
# -------------------------------------------------------

data(iris)    

par(mar=c(0,0,1,0)) 

# Get a sample from the iris data set
# Randomly choose 40 observations from iris
iris_sample <- iris[sample(1:150, 40),]

distance_metric <- dist(iris_sample[,-5], method="euclidean")  # dist object

# Using hclust() from stats package using "average" cluster method
cluster <- hclust(distance_metric, method="average")

# Plot the cluster dendrogram
plot(cluster, hang=-1, label=iris_sample$Species, main="Iris Data Set Clusters")


# -------------------------------------------------------
# K-means Clustering
# -------------------------------------------------------

# K-means clustering algorithm:
# Specify starting centroids
# Assign to closest centroid
# Recalculate centroids
# Reassign values
# Update centroids

df <- data.frame(x,y)

# Number of initial centroids=3

# Need to set seed to get sames results because kmeans() uses a
# random number generator to come up with the centers if you use the
# centers argument
set.seed(42)

# Initial number of centroids=3
kmeans1 <- kmeans(df,centers=3, iter.max=10)   # Create a kmeans object
names(kmeans1)
#[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#[6] "betweenss"    "size"         "iter"         "ifault"   

# Show which cluster each data point assigned to
kmeans1$cluster     # This is an integer vector
# [1] 3 3 3 3 1 1 1 1 2 2 2 2
kmeans1$centers
#           x         y
# 1 1.9906904 2.0078229
# 2 2.8534966 0.9831222
# 3 0.8904553 1.0068707

# Plot clusters
par(mar=rep(0.2,4))
plot(x,y,col=kmeans1$cluster,pch=19,cex=2)
points(kmeans1$centers,col=1:4,pch=3,cex=3,lwd=3)


# -------------------------------------------------------
# Principal Component Analysis 
# -------------------------------------------------------

data(USArrests)

names(USArrests)
# [1] "Murder"   "Assault"  "UrbanPop" "Rape"    

# Discover basis statistics for the data set
summary(USArrests)
#    Murder          Assault         UrbanPop          Rape      
#Min.   : 0.800   Min.   : 45.0   Min.   :32.00   Min.   : 7.30  
#1st Qu.: 4.075   1st Qu.:109.0   1st Qu.:54.50   1st Qu.:15.07  
#Median : 7.250   Median :159.0   Median :66.00   Median :20.10  
#Mean   : 7.788   Mean   :170.8   Mean   :65.54   Mean   :21.23  
#3rd Qu.:11.250   3rd Qu.:249.0   3rd Qu.:77.75   3rd Qu.:26.18  
#Max.   :17.400   Max.   :337.0   Max.   :91.00   Max.   :46.00 

apply(USArrests, 2, mean)      # Vastly different means for columns(2)

apply(USArrests, 2, var)       # Vastly different variances
#  Murder    Assault   UrbanPop       Rape 
#18.97047 6945.16571  209.51878   87.72916 


# scale=T for sd=1, center=T for mean 0
pca <- prcomp(USArrests, scale=TRUE, center=TRUE)

names(pca)                  # PCA yields output variables
# [1] "sdev"     "rotation" "center"   "scale"    "x"

pca$center                  # Mean of variables used for scaling
#Murder  Assault UrbanPop     Rape 
# 7.788  170.760   65.540   21.232 

pca$scale                   # Standard deviation of variables
#  Murder   Assault  UrbanPop      Rape 
#4.355510 83.337661 14.474763  9.366385 

pca$scale^2                 # Variance of variables
#  Murder    Assault   UrbanPop       Rape 
#18.97047 6945.16571  209.51878   87.72916

pca$rotation 
#                PC1        PC2        PC3         PC4
#Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
#Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
#UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
#Rape     -0.5434321 -0.1673186  0.8177779  0.08902432

# x: rows are states, columns are PC1, PC2, PC3, PC4 scores
dim(pca$x)                  # 50x4

# Plot the first 2 PCs
biplot(pca, scale=0)        # Plot the first two PCs

# Variance explained by each PC
pca_var <- pca$sdev^2
pca_var
#[1] 2.4802416 0.9897652 0.3565632 0.1734301

# Proportion of variance explained by each PC
pve <- pca_var/sum(pca_var)
pve
# [1] 0.62006039 0.24744129 0.08914080 0.04335752
# PC1 explains 62.0% of the variance in the data
# PC2 explains 24.7% of the variance in the data

# Plot PVE by each PC (scree plot) and cummulative PVE
par(mfrow=c(1,2))
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')


d1 <- predict(pca)        # 50x4, contains PC1, PC2, PC3, PC4

# 50x4 same as above
d2 <- scale(USArrests, pca$center, pca$scale) %*% pca$rotation

# Use PC1, PC2, PC3 to get 95% of the variance
# Now we have a reduced dimension data set
d3 <- predict(pca)[,1:3]    # 50X3

d4 <- scale(USArrests,pca$center, pca$scale) %*% pca$rotation[,1:3]


# 3D plot using 3 principal components, 96% of variation
library(scatterplot3d)
scatterplot3d(d4[,1],d4[,2],d4[,3], main="3 Principal Components")

# 2D plot using 2 principal components, 87% of variation

d5 <- predict(pca)[,1:2]    # 50X2

plot(d5[,1], d5[,2], col="blue", main="2 principal components")

plot(pca$x[,1], pca$x[,2], main="2 principal components")








