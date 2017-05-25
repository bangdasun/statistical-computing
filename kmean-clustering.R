# K-means clustering 
# @author: Bangda Sun
# @date: 05/18/2017

# K-means clustering algorithm is a special case of EM algorithm, 
# which is one of algorithms to estimate the parameter of mixture model,
# also can be viewed as a special kind of clustering

### Algorithm ###
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

### Build-in function
km = kmeans(df, centers = 2, nstart = 10)
km

df$cluster1 = factor(km$cluster)
ggplot(df) + geom_point(mapping = aes(x = x, y = y, colour = cluster1), size = 2)

### Step by Step
dist = function (x, y) {
  d = sqrt((x[1] - x[2])^2 + (y[1] - y[2])^2)
  return(d)
}
