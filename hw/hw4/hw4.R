# i
set.seed(1)
setwd("C://Users//Bangda//Desktop//GR5206 Materials//Hwk4")
gmp <- read.table("gmp.txt", header = TRUE)
head(gmp)
gmp$pop <- round(gmp$gmp/gmp$pcgmp)
plot(gmp$pop, gmp$pcgmp, log = "x", ylab = "Per-capita", xlab = "Population")
curve(6611*x^(1/8), add = TRUE, col = "red")
curve(6611*x^(0.1), add = TRUE, col = "blue")
curve(6611*x^(0.15), add = TRUE, col = "green")
legend(x = "topleft", legend = c("beta1 = 0.125", "beta1 = 0.1", "beta1 = 0.15"),
       col = c("red", "blue", "green"), lwd = 2, pch = c(NA, NA, NA))

# ii
mse <- function(beta, X = gmp$pop, Y = gmp$pcgmp){
  # The arguments of the function are
  # 1. beta0 and beta1 in a vector
  # 2. X: (default) population values
  # 3. Y: (default) per-capita CMP
  # return the mse 
  stopifnot(length(beta) == 2)
  sse <- sum((Y - beta[1]*X^(beta[2]))^2)
  mse <- sse / length(Y)
  return(mse)
}
mse(c(6611, 0.15))
mse(c(5000, 0.10))

# iii
est1 <- nlm(mse, c(beta0 = 6611, beta1 = 1/8))
est2 <- nlm(mse, c(beta0 = 6611, beta1 = 0.10))
est3 <- nlm(mse, c(beta0 = 6611, beta1 = 0.15))

list(est1.mini = est1$minimum, est1.ests = est1$estimate, 
     est2.mini = est2$minimum, est2.ests = est2$estimate, 
     est3.mini = est3$minimum, est3.ests = est3$estimate)

# iv
plm <- function(beta0, beta1, X = gmp$pop, Y = gmp$pcgmp){
  # This function has 4 arguments:
  # 1. initial value of beta0
  # 2. initial value of beta1
  # 3. X: (default) population values
  # 4. Y: (default) per-capita CMP
  # return the list including:
  # 1. final value of beta0
  # 2. final value of beta1
  # 3. final value of MSE
  stopifnot(length(beta0) == 1, length(beta1) == 1)
  
  # use nlm() to minimize the mse, the input of mse() is same as the plm()
  est        <- nlm(mse, c(beta0, beta1), X = X, Y = Y)
  
  # return the optimal value of beta0 and beta1 and the mse
  returnlist <- list(est.beta0 = est$estimate[1],
                     est.beta1 = est$estimate[2],
                     mse = est$minimum)
  return(returnlist)
}
# start from (6611, 0.15)
plm(6611, 0.15)
# start from (5000, 0.1)
plm(5000, 0.10)

# v - a
pcgmp.mean <- mean(gmp$pcgmp); pcgmp.mean
pcgmp.sd <- sd(gmp$pcgmp); pcgmp.sd
pcgmp.se <- pcgmp.sd / sqrt(length(gmp$pcgmp)); pcgmp.se

# v - b
mean.cal <- function(indices){
  # this function create one bootstrap sample
  n <- dim(gmp)[1]
  stopifnot(length(indices) == n)
  mean.cal <- mean(gmp$pcgmp[indices])
  return(mean.cal)
}

# v - c
bootstrap.mean <- rep(NA, 100)
for (i in 1:100){
  indices <- sample(1:dim(gmp)[1], replace = TRUE)
  bootstrap.mean[i] <- mean.cal(indices)
}
head(bootstrap.mean)

# v - d
bootstrap.se <- sd(bootstrap.mean); bootstrap.se

# vi
plm.bootstrap <- function(beta0, beta1, X = gmp$pop, Y = gmp$pcgmp, B = 100){
  # this function compute the bootstrap estimate of standard error of beta0 and beta1
  # former 4 arguments are same as plm() 
  # las argument is the number of bootstrap samples with default 100
  
  bootstrap.sample <- matrix(rep(NA, B*2), ncol = 2)
  for (i in 1:B){
    indices <- sample(1:length(X), replace = TRUE)
    est <- plm(beta0, beta1, X = X[indices], Y = Y[indices])
    bootstrap.sample[i,1] <- est$est.beta0
    bootstrap.sample[i,2] <- est$est.beta1
  }
  beta0.se <- sd(bootstrap.sample[,1])
  beta1.se <- sd(bootstrap.sample[,2])
  est.se   <- list(bootstrap.se.beta0 = beta0.se, bootstrap.se.beta1 = beta1.se)
  return(est.se)
}
plm.bootstrap(6611, 0.15)
plm.bootstrap(5000, 0.10)

# vii
gmp2013 <- read.table("gmp-2013.txt", header = TRUE)
gmp2013$pop <- round(gmp2013$gmp/gmp2013$pcgmp)
plm(beta0 = 6611, beta1 = 0.15, X = gmp2013$pop, Y = gmp2013$pcgmp)
plm.bootstrap(beta0 = 6611, beta1 = 0.15, X = gmp2013$pop, Y = gmp2013$pcgmp)
plm(beta0 = 5000, beta1 = 0.10, X = gmp2013$pop, Y = gmp2013$pcgmp)
plm.bootstrap(beta0 = 5000, beta1 = 0.10, X = gmp2013$pop, Y = gmp2013$pcgmp)
