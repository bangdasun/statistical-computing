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
df$cluster3 = factor(ifelse(dist1 < dist2, 1, 2))

p3 = ggplot() + 
  geom_point(data = df, mapping = aes(x = x, y = y, colour = cluster3), size = 2) +
  labs(title = 3) +
  geom_point(mapping = aes(x = center1_1[1], y = center1_1[2]), shape = 3, size = 3, color = "red") +
  geom_point(mapping = aes(x = center2_1[1], y = center2_1[2]), shape = 3, size = 3, color = "blue")

center1_2 = c(mean(df$x[df$cluster3 == 1]), mean(df$y[df$cluster3 == 1]))
center2_2 = c(mean(df$x[df$cluster3 == 2]), mean(df$y[df$cluster3 == 2]))

p4 = ggplot() + 
  geom_point(data = df, mapping = aes(x = x, y = y, colour = cluster3), size = 2) + 
  labs(title = 4) +
  geom_point(mapping = aes(x = center1_2[1], y = center1_2[2]), shape = 3, size = 3, color = "red") +
  geom_point(mapping = aes(x = center2_2[1], y = center2_2[2]), shape = 3, size = 3, color = "blue")

library(grid)
library(gridExtra)
grid.arrange(p1, p2, p3, p4, ncol = 2)
