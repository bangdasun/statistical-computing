###############################
## Lecture 11 Student Script ##
###############################

## Section I: Split/Apply/Combine ##
# iteration in R without for() !
setwd("C://Users//Bangda//Desktop//GR5206 Materials")

# The Strikes Data
strikes <- read.csv("strikes.csv", as.is = TRUE)
dim(strikes) 
head(strikes, 3)

italy.strikes <- subset(strikes, country == "Italy")
# Equivalently,
italy.strikes <- strikes[strikes$country == "Italy", ]
dim(italy.strikes)

head(italy.strikes, 5)

italy.fit <- lm(strike.volume ~ left.parliament, data = italy.strikes)
plot(strike.volume~left.parliament, data = italy.strikes, main="Italy Strike Volume Versus Leftwing Alignment", ylab = "Strike volume", xlab = "Leftwing Alignment")
abline(italy.fit, col = 2)

my.strike.lm <- function(country.df) {
  return(lm(strike.volume ~ left.parliament, data = country.df)$coeff)
}
my.strike.lm(subset(strikes, country == "Italy"))

strike.coef  <- NULL
my.countries <- c("France", "Italy", "USA")
for (this.country in my.countries) {
  country.dat <- subset(strikes, country == this.country)
  new.coefs   <- my.strike.lm(country.dat)
  strike.coef <- cbind(strike.coef, new.coefs)
}
colnames(strike.coef) <- my.countries
strike.coef

# Splitting the data with split()
strikes.split <- split(strikes, strikes$country)
names(strikes.split)

# Now the apply step
strike.coef <- sapply(strikes.split[1:12], my.strike.lm)
strike.coef

plot(1:ncol(strike.coef), strike.coef[2, ], xaxt = "n", xlab = "", ylab = "Regression coefficient", main="Countrywise labor activity by leftwing score")
axis(side = 1, at = 1:ncol(strike.coef), labels = colnames(strike.coef), las = 2, cex.axis = 0.5)
abline(h = 0, col = "grey")

## task 1
## 1)
new.strikes <- data.frame(strikes$year, strikes$unemployment,
                          strikes$inflation, strikes$strike.volume)
colnames(new.strikes) <- c("year", "unemployment.rate", "inflation rate", "strike volumn")

year.split <- split(new.strikes, new.strikes$year)
ave <- sapply(year.split, FUN = mean)

## Section II: Using plyr ##

library(plyr)
# a*ply()
my.array           <- array(1:27, c(3,3,3))
rownames(my.array) <- c("R1", "R2", "R3")
colnames(my.array) <- c("C1", "C2", "C3")
dimnames(my.array)[[3]] <- c("Bart", "Lisa", "Maggie")


my.array 
my.array[, , 3] # Maggie's sheet

# install.packages("plyr")
aaply(my.array, 1, sum) # Get back an array
adply(my.array, 1, sum) # Get back a data frame
alply(my.array, 1, sum) # Get back a list
aaply(my.array, 2:3, sum) # Get back a 3 x 3 array
adply(my.array, 2:3, sum) # Get back a data frame
alply(my.array, 2:3, sum) # Get back a list

aaply(my.array, c(1,3), sum)
adply(my.array, c(1,3), sum)


# l*ply()
my.list <- list(nums = rnorm(1000), lets = letters, pops = state.x77[ ,"Population"])
head(my.list[[1]], 5)
head(my.list[[2]], 5)
head(my.list[[3]], 5)

laply(my.list, range) # Get back an array
ldply(my.list, range) # Get back a data frame
llply(my.list, range) # Get back a list
laply(my.list, summary) 
ldply(my.list, summary)
llply(my.list, summary) # Works just fine

# The drop symbol
par(mfrow = c(3, 3), mar = c(3.5,3.5,1,1))
a_ply(my.array, 2:3, plot, ylim = range(my.array), pch = 19, col = 6)

# d*ply()

# Getting regression coefficients separately for each country, old way:
strikes.list  <- split(strikes, f = strikes$country)
strikes.coefs <- sapply(strikes.list, my.strike.lm)
strikes.coefs[, 1:12]

# Getting regression coefficient separately for each country, new way, in three formats:
strike.coef.a <- daply(strikes, .(country), my.strike.lm)

# Get back an array, note the difference to sapply()
head(strike.coef.a)

strike.coef.d <- ddply(strikes, .(country), my.strike.lm)
head(strike.coef.d) # Get back a data frame

strike.coef.l <- dlply(strikes, .(country), my.strike.lm)
head(strike.coef.l, 3) # Get back a list

# First create a variable that indicates whether the year is pre 1975, and add it to the data frame

strikes$yearPre1975 <- strikes$year <= 1975

# Then use (say) ddply() to compute regression coefficients for each country pre & post 1975
  
strike.coef.75 <- ddply(strikes, .(country, yearPre1975), my.strike.lm)
dim(strike.coef.75) # Note there are 18 x 2 = 36 rows

head(strike.coef.75)

# Can also create factor variables on-the-fly with I()
strike.coef.75 <- ddply(strikes, .(country, I(year<=1975)), my.strike.lm)
dim(strike.coef.75) # Again, 18 x 2 = 36 rows

head(strike.coef.75)

## task 2
# 1

## Section III: Reshaping Dataframes ##

snoq <- read.csv("snoqualmie.csv", header = FALSE, as.is = TRUE)
colnames(snoq) <- 1:366
snoq$year      <- 1948:1983

dim(snoq)
snoq[1:3, 1:10]
snoq[1:3, 360:367]

# install.packages("reshape2")
require(reshape2)
snoq.melt <- melt(snoq, id.vars = "year", variable.name = "day", value.name = "precip")
head(snoq.melt)
tail(snoq.melt)
dim(snoq.melt) # 36*366

# order by years
snoq.melt.chron <- snoq.melt[order(snoq.melt$year, snoq.melt$day), ]
head(snoq.melt.chron)

leap.days <- snoq.melt.chron$day == 366
sum(is.na(snoq.melt.chron$precip[leap.days]))

snoq.melt.chron <- na.omit(snoq.melt.chron)

snoq.recast <- dcast(snoq.melt, year ~ ...)
dim(snoq.recast)
snoq.recast[1:4, 1:15]


## Section IV: Unsupervised Learning ##

## Section V: PCA ##

# install.packages("ISLR")
library("ISLR")
head(USArrests)

USArrests <- apply(USArrests, 2, scale)
head(USArrests)

pca <- prcomp(USArrests)
pca
biplot(pca)



## Section VI: K-Means Clustering ##

set.seed(2)

# Create a random matrix where the first column is N(3, 1) and the 2nd column N(-4, 1)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
head(x, 5)

km.out <- kmeans(x, centers = 2, nstart = 20)

# Cluster assignments for the 50 observations
km.out$cluster[1:25]
km.out$cluster[26:50]

plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K=2", xlab = "", ylab = "", pch = 20, cex = 2)
points(km.out$centers[1,1], km.out$centers[1,2], pch = 4, cex = 2, col = "black")
points(km.out$centers[1,1], km.out$centers[1,2], pch = 1, cex = 2, col = "black")
text(km.out$centers[1,1] + .5, km.out$centers[1,2] + .5, "center 1")
points(km.out$centers[2,1], km.out$centers[2,2], pch = 4, cex = 2, col = "black")
points(km.out$centers[2,1], km.out$centers[2,2], pch = 1, cex = 2, col = "black")
text(km.out$centers[2,1] + .5, km.out$centers[2,2] + .5, "center 2")


# K=3
set.seed(4)
km.out <- kmeans(x, centers = 3, nstart = 20)
km.out$cluster

# Initial Clusterings Matter
set.seed(3)
km.out <- kmeans(x, centers = 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, centers = 3, nstart = 20)
km.out$tot.withinss

# k-mean function
# authort: Bangda Sun
K_means_cluster <- function(data, k = 2, max.iter = 10){
  # k: number of clusters
  
  data <- as.data.frame(data)
  
  # labels of clusters
  label <- 1:k
  
  # number of observations and features
  n <- dim(data)[1]
  p <- dim(data)[2]
  
  # assign clusters randomly
  data$cluster <- sample(1:k, size = n, replace = TRUE)
  
  # initialize centroid
  centroid <- matrix(rep(NA, k * p), ncol = p)
  
  # iteration
  while (mean(sqrt((old.centroid - centroid)^2)) > 1e-4){}
  # calculate the centroid
  for (i in 1:k){
    centroid[i,] <- colMeans(data[data$cluster == i, -dim(data)[2]])
  }
  
  # require function: mini.distance.R
  mini.dist <- mini.distance(data = data[,-dim(data)[2]], point = centroid)
  data$cluster <- mini.dist$cluster
  # update centroid
  old.centroid <- centroid
    
  plot(data[,1],data[,2],col = factor(data[,3]))
  points(centroid[1,1], centroid[1,2], pch = 4)
  points(centroid[2,1], centroid[2,2], pch = 4)
  
  
}

mini.distance <- function(data, point){
  # calculate the Euclidean distance between each point and centroid
  # and select the minimum
  
  # transform to dataframe
  data <- as.data.frame(data)
  
  # k: number of centroid
  k <- dim(point)[1]
  
  # dimension of data
  p <- dim(data)[2]
  
  # number of obs
  n <- dim(data)[1]
  
  # calculate the distance of each point to the centroid
  distance <- matrix(NA, ncol = k, nrow = n)
  for (i in 1:n){
    for (j in 1:p){
      for (m in 1:k)
      {
        distance[i, m] <- sqrt(sum((data[i, j] - point[m, j])^2))
      }
    }
  }
  min.dist    <- apply(distance, 1, min)
  which.point <- apply(distance, 1, which.min)
  mini.dist   <- data.frame(distance, cluster = which.point) 
  return(mini.dist)
}


