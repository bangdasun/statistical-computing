# K-means clustering 
# @author: Bangda Sun
# @date: 05/18/2017

# K-means clustering algorithm is a special case of EM algorithm, 
# which is one of algorithms to estimate the parameter of mixture model,
# also can be viewed as a special kind of clustering

### ========================== Algorithm ========================== ###
# 1. Initialize the assignment of every point
# 2. while (prev_cluster != curr_cluster):
#       calculate the center of each cluster
#       calculate the distance of every point to all centers
#       assign the point to the nearest center

# Data
x = rnorm(20, mean = 3, sd = 1)
y = rnorm(20, mean = 4, sd = 2)
y[x < 3] = y[x < 3] + 10
df = data.frame(x = x, y = y)

library(ggplot2)
ggplot(df) + geom_point(mapping = aes(x = x, y = y), size = 2)

### ========================== Build-in function  ========================== ###
km = kmeans(df, centers = 2, nstart = 10)
km

df$cluster1 = factor(km$cluster)
ggplot(df) + geom_point(mapping = aes(x = x, y = y, colour = cluster1), size = 2)


###  ========================== Step by Step  ========================== ###
dist = function (x, y) {
  d = sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2)
  return(d)
}

# Initialization
df$cluster2 = factor(sample(c(1,2), nrow(df), replace = TRUE))
p1 = ggplot(df) + 
  geom_point(mapping = aes(x = x, y = y, colour = cluster2), size = 2) + 
  labs(title = "1")

# Calculate center and distance
center1_1 = c(mean(df$x[df$cluster2 == 1]), mean(df$y[df$cluster2 == 1]))
center2_1 = c(mean(df$x[df$cluster2 == 2]), mean(df$y[df$cluster2 == 2]))
p2 = ggplot() + 
  geom_point(data = df, mapping = aes(x = x, y = y, colour = cluster2), size = 2) + 
  labs(title = 2) +
  geom_point(mapping = aes(x = center1_1[1], y = center1_1[2]), shape = 3, size = 3, color = "red") +
  geom_point(mapping = aes(x = center2_1[1], y = center2_1[2]), shape = 3, size = 3, color = "blue")

dist1 = apply(df[, c("x", "y")], MARGIN = 1, dist, center1_1)
dist2 = apply(df[, c("x", "y")], MARGIN = 1, dist, center2_1)

# Assign point to nearest center
df$cluster3 = factor(ifelse(dist1 < dist2, 1, 2))

p3 = ggplot() + 
  geom_point(data = df, mapping = aes(x = x, y = y, colour = cluster3), size = 2) +
  labs(title = 3) +
  geom_point(mapping = aes(x = center1_1[1], y = center1_1[2]), shape = 3, size = 3, color = "red") +
  geom_point(mapping = aes(x = center2_1[1], y = center2_1[2]), shape = 3, size = 3, color = "blue")

# Update centers
center1_2 = c(mean(df$x[df$cluster3 == 1]), mean(df$y[df$cluster3 == 1]))
center2_2 = c(mean(df$x[df$cluster3 == 2]), mean(df$y[df$cluster3 == 2]))

# Check if the clusters are unchange
p4 = ggplot() + 
  geom_point(data = df, mapping = aes(x = x, y = y, colour = cluster3), size = 2) + 
  labs(title = 4) +
  geom_point(mapping = aes(x = center1_2[1], y = center1_2[2]), shape = 3, size = 3, color = "red") +
  geom_point(mapping = aes(x = center2_2[1], y = center2_2[2]), shape = 3, size = 3, color = "blue")

library(grid)
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)

###  ======================= Self-defined function  ======================= ###
set.seed(1234)
x = rnorm(40, mean = 3, sd = 1)
y = rnorm(40, mean = 4, sd = 2)
y[x < 3] = y[x < 3] + 10
X <- cbind(x, y)
df <- as.data.frame(X)

library(ggplot2)
ggplot(df) + geom_point(mapping = aes(x = x, y = y), size = 2)


calcDistance <- function(x, center) {
  # @param x: sample
  # @param center: center of cluster
  if (length(x) != length(center)) stop('x and center should have same length.')
  
  x      <- matrix(x, nrow = 1)
  center <- matrix(center, nrow = 1)
  return( (x - center) %*% t((x - center)) )
}

assignCluster <- function(x, Center) {
  # @param x: sample
  # @param Center: with shape k x p
  dist <- rep(0, nrow(Center))
  for (i in 1:nrow(Center)) {
    dist[i] <- calcDistance(x, Center[i, ])
  }
  return( which.min(dist) )
}

calcCenter <- function(X, cluster) {
  # @param X: data matrix
  # @param cluster: vector of cluster assignment
  Center <- matrix(0, nrow = length(unique(cluster)), ncol = ncol(X))
  for (i in unique(cluster)) {
    Center[i, ] <- colMeans(X[which(cluster == i), ])
  }
  return(Center)
}

my_kmeans <- function(X, k = 2) {
  # @param X: data matrix with shape n x p 
  # @param k: number of clusters
  
  Center <- matrix(0, nrow = k, ncol = ncol(X))
  clusterAssignMat <- matrix(0, nrow = nrow(X), ncol = 1)
  prevCluster <- clusterAssignMat
  
  currentCluster <- sample(1:k, size = nrow(X), replace = TRUE)
  
  while(!identical(prevCluster, currentCluster)) {
    clusterAssignMat <- cbind(clusterAssignMat, currentCluster)
    prevCluster <- currentCluster
    # check again if cluster unchanged
    if (identical(prevCluster, currentCluster)) {
      break
    }
    Center <- calcCenter(X, currentCluster)
    currentCluster <- apply(X, 1, assignCluster, Center = Center)
  }
  return(list(cluster = currentCluster, Center = Center, cluster_iter = clusterAssignMat))
}

kmeansResult <- my_kmeans(X, k = 2)

df$cluster <- kmeansResult$cluster
ggplot(df) + geom_point(mapping = aes(x = x, y = y, color = factor(cluster)), size = 2)
