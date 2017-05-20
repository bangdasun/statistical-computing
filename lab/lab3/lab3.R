# install.packages("ISLR")
library(ISLR)

# view data
head(Weekly)
# scatter plot
plot(Weekly$Today, Weekly$Lag2, xlab = "Today", ylab = "Lag2", main = "Relationship between the considered week's return and \nthe previous two week's return")
# side by side boxplot
boxplot(Weekly$Lag1~Weekly$Direction, main = "Box plots of percentage return of one week previous")

KNN.Decision <- function(predictor, K = 5, training.set){
  # this function has 3 parameters:
  #   predictor: new lag1 - lag5, should be a length 5 vector
  #   K: the number of closet points, default 5
  #   training.set: existed Lag1 - Lag5, as the training data
  # this function will return the prediction direction (up or down)
  
  # numbers of observations
  n               <- dim(training.set)[1]
  
  # set the requirement of parameters 
  stopifnot(length(predictor) == 5, length(K) == 1, dim(training.set)[2] == 5, K <= n) 
  
  # make sure the mode of predictor is numeric
  predictor       <- as.numeric(predictor)
  
  # compute the euclidean distance
  dist            <- sqrt((training.set[,1] - predictor[1])^2+
                          (training.set[,2] - predictor[2])^2+
                          (training.set[,3] - predictor[3])^2+
                          (training.set[,4] - predictor[4])^2+
                          (training.set[,5] - predictor[5])^2)
  
  # sort the distance and select top K nearst
  neighbor        <- order(dist)[1:K]
  
  # find these K points direction
  neighbor.dir    <- Weekly$Direction[neighbor]
  
  # predict the direction
  prediction.dir  <- names(which.max(table(neighbor.dir)))
  return(prediction.dir)
}
# prediction
predictor <- c(-.5, .5, -.5, -.5, .5)
KNN.Decision(predictor, training.set = Weekly[,2:6])

# test data
test <- Weekly[Weekly$Year <= 2010 & Weekly$Year >= 2009, ]
# train data
train <- Weekly[Weekly$Year <= 2008 & Weekly$Year >= 1990, ]
# number of test data
n <- dim(test)[1]
prediction.dir <- rep(NA, n)
for (i in 1:n){
  prediction.dir[i] <- KNN.Decision(predictor = test[i, 2:6], K = 5, training.set = train[,2:6])
}
# prediction of direction 
head(prediction.dir)
# test error
test.error <- mean(prediction.dir != test$Direction); test.error

n <- dim(test)[1]
prediction.dir <- rep(NA, n)
for (i in 1:n){
  prediction.dir[i] <- KNN.Decision(predictor = test[i, 2:6], K = 3, training.set = train[,2:6])
}
# prediction of direction 
head(prediction.dir)
# test error
test.error <- mean(prediction.dir != test$Direction); test.error

fold <- rep(c(1:9), 121)
fold <- sample(fold, replace = FALSE)
table(fold)

prediction.dir <- matrix(rep(NA, 1089), nrow = 9, ncol = 121)
test.error     <- rep(NA, 9)
Weekly$group   <- fold
for (i in 1:9){
  test  <- Weekly[Weekly$group == i,]
  train <- Weekly[Weekly$group != i,]
  for (j in 1:121){
    prediction.dir[i, j] <- KNN.Decision(predictor = test[j, 2:6], K = 5, training.set = train[,2:6])
  }
  test.error[i] <- mean(prediction.dir[i,] != test$Direction)
}
cv <- mean(test.error); cv

ptm <- proc.time()
prediction.dir <- matrix(rep(NA, 1089), nrow = 9, ncol = 121)
test.error1    <- rep(NA, 9)
Weekly$group   <- fold
cv             <- rep(NA, 3)
K              <- c(1,3,7)
for (k in 1:3){
  for (i in 1:9){
    test  <- Weekly[Weekly$group == i,]
    train <- Weekly[Weekly$group != i,]
    for (j in 1:121){
      prediction.dir[i, j] <- KNN.Decision(predictor = test[j, 2:6], K = K[k], training.set = train[,2:6])
    }
    test.error1[i] <- mean(prediction.dir[i,] != test$Direction)
  }
  cv[k] <- mean(test.error1)
}
cv
proc.time() - ptm
