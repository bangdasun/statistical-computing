##############################
## Lecture 5 Student Script ##
##############################

# review: web scrapping

# we've learned about getting data in and out of R when it's strucutred: read.table(), read.csv()
# ofter, like the last example, it's not structured: could have metea data

# how do we use the computer extract the information we want?
# in HTML code

# example of a amazon web page


## Section I: Functions ##

# Significance Function

significant <- function(x) {
  if (x <= 0.05) { return(TRUE) } 
  else { return(FALSE) }
}

significant(0.04)
significant(0.09)


# Robust Loss Function

# Inputs:  A vector of numbers (x)
# Outputs: A loss vector with x^2 for small elements, 
#          and 2|x|-1 for large ones

res_loss <- function(x) {
  loss_vec <- ifelse(x^2 > 1, 2*abs(x) - 1, x^2)
  return(loss_vec)
}

vec <- c(-0.5, 0.9, -3, 4)
res_loss(vec)

# task: write a function takes as input a vector of numerical values and 
# return 5 times the sum of those values
FiveTimesSum <- function(x){
  sum <- 5 * sum(x)
  return(sum)
}

# Named Arguments

# Inputs: A vector of numbers (x), crossover location (c)
# Outputs: A loss vector with x^2 for small elements, 
#          and 2c|x|-c for large ones

res_loss2 <- function(x, c = 1) {
  loss_vec <- ifelse(x^2 > c, 2*c*abs(x) - c, x^2)
  return(loss_vec)
}

identical(res_loss(vec), res_loss2(vec, c=1))
identical(res_loss(vec), res_loss2(vec, c=2))

# Default values get used if names are missing:
identical(res_loss2(vec, c=1), res_loss2(vec))

# Named argument can go in any order when they are explicitly tagged:
identical(res_loss2(x=vec, c=2), res_loss2(c=2, x=vec))


# Funny things happen when the input aren't as we expect
vec <- c(-0.5, 0.9, -3, 4)
res_loss2(vec, c = c(1,1,1,5))
res_loss2(vec, c = -1)

# Checking Arguments

res_loss2 <- function(x, c = 1) {
  # Scale should be a single positive number
  stopifnot(length(c) == 1, c > 0)                         # stopifnot function
  loss_vec <- ifelse(x^2 > c^2, 2*c*abs(x) - c, x^2)
  return(loss_vec)
}

# check arguments
res_loss2(vec, c = c(1,1,1,5))
res_loss2(vec, c = -1)

# task
KTimesSum <- function(c, K = 5){
  stopifnot(length(K) == 1, K > 0)
  return(K * sum(c))
}
KTimesSum(1:3)
KTimesSum(1:3, K = 10)

# Use Your Function in Other Functions

curve(res_loss2, from = -2, to = 2)
g <- Vectorize(KTimesSum)                      # why?
curve(g, from = 0, to = 10)

# The Function Environment
# 1. Each function has its own(internal) environment
# 2. Names in the function environment override names from the global environment
# 3. Assignments in the internal environment don't change the global environment
# 4. Functions search for named variables in the environment in which the function was created

x <- 7                                          # a value in environment
vec <- 1:7                                      
y <- c("dog", "cat")
addition <- function(y) {x <- x + y; return(x)}
addition(1)
x
y


circle.area <- function(r) {return(pi*r^2)}
circle.area(1:3)

true.pi <- pi # Save the real value
pi      <- 3 # Assign a new value
circle.area(1:3)

pi <- true.pi # Restore the real value
circle.area(1:3)



## Section II: Fitting a Model ##



# Getting the data

gmp <- read.table("C://Users//Bangda//Desktop//GR5206 Materials//gmp.txt", 
                  as.is = TRUE, header = TRUE)
head(gmp)
gmp$pop <- gmp$gmp/gmp$pcgmp
head(gmp)

# Plotting the data

plot(gmp$pop, gmp$pcgmp, log = "x", xlab = "Population", ylab = "Per-capita Economic Output")

# An example of the model with beta_0 = 6611, beta_1 = 1/8
curve(6611*x^{1/8}, add = TRUE, col = "blue") 

# A First Attempt

# gradient descent
# 1. start at a point x0
# 2. calculate the derivative at the point, f'(x0)
# 3. take a step in the oppsite direction of the derivative to find x1
# 4. repeat
# 5. stop when the derivatives is small enough or at some set number of iterations


# Parameters
max.iter         <- 100     # How long we run the alg.
stop.deriv       <- 1/100   # If derivative is small, stop
deriv.step       <- 1/1000  # This is h
step.scale       <- 1e-12   # This is c

# Initializations
iter             <- 0       # Compare to max.iteration     
deriv            <- Inf     # Compare to stop.deriv
beta             <- 0.15

while((iter < max.iter) & (deriv > stop.deriv)) {
  iter  <- iter + 1
  sse.1 <- mean((gmp$pcgmp - 6611*gmp$pop^beta)^2)
  sse.2 <- mean((gmp$pcgmp - 6611*gmp$pop^(beta + deriv.step))^2)
  deriv <- (sse.2 - sse.1)/deriv.step
  beta  <- beta - step.scale*deriv
}
list(beta = beta, iteration = iter, 
     converged = (iter < max.iter))
# First Fix
est.scaling.exponent <- function(beta){
  max.iter   <- 100     # how long should we run the algorithm
  stop.deriv <- 1/100   # if derivative is small, stop
  deriv.step <- 1/1000  # this is h
  step.scale <- 1e-12   # this is c
  iter       <- 0       # initial iteration
  deriv      <- Inf     # initial derivative
  while((iter < max.iter) & (abs(deriv) > stop.deriv)) {
    iter  <- iter + 1
    mse.1 <- mean((gmp$pcgmp - 6611*gmp$pop^beta)^2)
    mse.2 <- mean((gmp$pcgmp - 6611*gmp$pop^(beta + deriv.step))^2)
    deriv <- (mse.2 - mse.1)/deriv.step
    beta  <- beta - step.scale*deriv
  }
  fit <- list(beta = beta, iteration = iter, 
       converged = (iter < max.iter))
  return(fit)
}

# Second Fix (set more parameters rather than assignment, easy to change the parameters)
# Problem of the first fix:
#   have to re-run if we want to change defined parameters
#   solutions: let's make them arguments (with default values) of the function
est.scaling.exponent <- function(beta, beta_0 = 6611, 
                                 max.iter = 100, stop.deriv = 0.01, deriv.step = 0.001,
                                 step.scale = 1e-12){
  iter  <- 0
  deriv <- Inf
  while ((iter < max.iter) & (abs(deriv) > stop.deriv)){
    iter  <- iter + 1
    mse.1 <- mean(sum(gmp$pcgmp - 6611 * gmp$pop^beta)^2)
    mse.2 <- mean(sum(gmp$pcgmp - 6611 * gmp$pop^(beta + deriv.step))^2)
    deriv <- (mse.2 - mse.1) / deriv.step
    beta <- beta - step.scale * deriv
  }
  fit <- list(beta = beta, iteration = iter, converged = (iter < max.iter))
  return(fit)
}


# Third Fix
# Problem of the second fix:
#   don't need to write out the MSE calculations twice in the body of the function
#   solutions: write a MSE() function
est.scaling.exponent <- function(beta, beta_0 = 6611,
                                 response = gmp$pcgmp, predictor = gmp$pop,
                                 max.iter = 100, stop.deriv = 0.01, deriv.step = 0.01,
                                 step.scale = 1e-12){
  iter <- 0
  deriv <- Inf
  mse <- function(b){
    mean((gmp$pcgmp - beta_0 * gmp$pop^b)^2)
  }
  while ((iter < max.iter) & (abs(deriv > stop.deriv))){
    iter <- iter + 1
    deriv <- (mse(beta + deriv.step) - mse(beta)) / deriv.step
    beta <- beta - step.scale * deriv
  }
  fit <- list(beta = beta, iteration = iter, converged = (iter < max.iter))
  return(fit)
}

# Problem: 
#   locked into using specific columns of gmp, if we want to use a different dataset, have to rewrite
#   solution: make them arguments

# Problem:
#   Want to make it easy for human to read
#   soltion: change the while loop to for loop with a break() command

# Fifth Fix
est.scaling.exponent <- function(beta, beta_0 = 6611, 
                                 response = gmp$pcgmp, predictor = gmp$pop, 
                                 max.iter = 100, stop.deriv = .01, deriv.step = .001, 
                                 step.scale =1e-12) {
  deriv <- Inf
  sse <- function(b) {mean((response - beta_0*predictor^b)^2)}
  for (i in 1:max.iter) {
    deriv <- (sse(beta + deriv.step) - sse(beta))/deriv.step
    beta  <- beta - step.scale*deriv 
    if (abs(deriv) < stop.deriv) {break()}
  }
  fit <- list(beta = beta, iteration = iter, 
              converged = (iter < max.iter))
  return(fit)
}  


##############################################################################################################
# Assessing model accuracy
# call grocery store data in lecture 2
Grocery <- read.table("C://Users//Bangda//Desktop//GR5206 Materials//Kutner_6_9.txt", header = T)
lm0 <- lm(Y ~ X1 + X2 + X3, data = Grocery)
summary(lm0)

# care error more on test dataset

# some questions:
# 1. the coefficients for numbers of cases was nearly 0, should this be included as a predictor in the model
# 2. suppose we have additional information like weather data, should it be included in the model?
# 3. is linear regression the best predictive model to use with this data?


############################################################################################################


## Section IV: Classifiers ##
# good classifiction: test error rate is smallest

# K Nearst Neighbors
#   In theory would like to always use Bayes classifier. In practice, don't know P(Y|X)!
#   KNN: 
#     1. estimate P(Y|X) and then classifies observations to the class with highest estimate prob
#     2. given a positive number k and a test observation set Xtest
#         1) identify k points in training data closet to Xtest, label Ntest
#         2) estimate conditional prob for class j as fraction of points in Ntest whose response 
#            values equal j
#         3) classify the test observation to class with largest prob


# As with regression, training error not a good predictor of test error

# Stock Market Example
install.packages("ISLR")
library(ISLR)
head(Smarket, 3)

# Variable code book:
#   Year: the year that the observation was recorded
#   LagX: percentage return X days ago
#   Volume: volume of shares traded (number of daily shares traded in billions)
#   Today: percentage return for today
#   Direction: a factor with levels down and up indicateign whether the market had a positive or negative return

mean(Smarket$Lag1[Smarket$Direction == "Up"])
mean(Smarket$Lag1[Smarket$Direction == "Down"])

plot(Smarket$Lag1, Smarket$Lag2, col = Smarket$Direction, 
     xlab="Lag1", ylab="Lag2", main="Today's Direction")
legend("bottomright", legend = levels(Smarket$Direction), 
       col=1:length(levels(Smarket$Direction)), pch=1)


# Coding the Procedure

K           <- 5
Lag1.new    <- 2
Lag2.new    <- 4.25         # K = 5 and new point (2, 4.25).

dists       <- sqrt((Smarket$Lag1 - Lag1.new)^2 + (Smarket$Lag2 - Lag2.new)^2)

neighbors   <- order(dists)[1:K]                    # return the index of top 5 nearst point
neighb.dir  <- Smarket$Direction[neighbors]         # get these points' direction 
choice      <- names(which.max(table(neighb.dir)))  # 
choice

# Bangda's code ##################################################################################
KNNclass <- function(Lag1new, Lag2new, K = 5){
  dists <- sqrt((Smarket$Lag1 - Lag1new)^2 + (Smarket$Lag2 - Lag2new)^2)
  
  neighbors     <- order(dists)[1:K]
  neighbors.dir <- Smarket$Direction[neighbors]
  choice        <- names(which.max(table(neighbors.dir)))
  return(choice)
}
KNNclass(Lag1new = 2, Lag2new = 4.25)

# Instructor's code##################################################################################
# A Function for the KNN Decision

KNN.decision <- function(Lag1.new, Lag2.new, K = 5, 
                         Lag1 = Smarket$Lag1, 
                         Lag2 = Smarket$Lag2) {
  ## remember the check of parameters !!!!!
  n <- length(Lag1)
  stopifnot(length(Lag2) == n, length(Lag1.new) == 1, 
            length(Lag2.new) == 1, K <= n)
  ##
  
  dists <- sqrt((Lag1-Lag1.new)^2 + (Lag2-Lag2.new)^2)
  
  neighbors  <- order(dists)[1:K]
  
  neighb.dir <- Smarket$Direction[neighbors]
  choice     <- names(which.max(table(neighb.dir)))
  return(choice)
}

# Using Test Data, K = 5

test  <- Smarket[Smarket$Year == 2005, ] 
train <- Smarket[Smarket$Year != 2005, ]

n.test <- nrow(test)
predictions <- rep(NA, n.test)

for (i in 1:n.test){
  predictions[i] <- KNN.decision(train$Lag1[i], 
                                 train$Lag2[i], Lag1 = train$Lag1, 
                                 Lag2 = train$Lag2) 
}

test.error <- sum(predictions != test$Direction)/n.test
test.error



# Using Test Data, K = 7

predictions <- rep(NA, n.test)

for (i in 1:n.test){
  predictions[i] <- KNN.decision(train$Lag1[i], 
                                 train$Lag2[i], K = 7, 
                                 Lag1 = train$Lag1, 
                                 Lag2 = train$Lag2) 
}

test.error <- sum(predictions != test$Direction)/n.test
test.error

# plot the test error
ptm <- proc.time()
nk <- 998
error <- rep(NA, nk)
predictions <- matrix(rep(NA, n.test * nk), nrow = nk)
for (j in 1:nk){
  for (i in 1:n.test){
    predictions[j, i] <- KNN.decision(train$Lag1[i], train$Lag2[i], K = j, Lag1 = train$Lag1, Lag2 = train$Lag2) 
  }
  test.error[j] <- sum(predictions[j, ] != test$Direction)/n.test
}
k <- 1:nk
plot(k, test.error, 'l')
proc.time() - ptm



