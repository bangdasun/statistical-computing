# i
n <- 100
p <- 10
s <- 3
set.seed(0)
x <- matrix(rnorm(n*p), n, p)
b <- c(-0.7, 0.7, 1, rep(0, p - s))
y <- x %*% b + rt(n, df = 2)

cor(x, y)

# ii
library(ggplot2)
xt <- seq(-6, 6, by = .001)
pdfdata <- data.frame(xt = xt, nd = dnorm(xt), td = dt(xt, df = 3))
ggplot(data = pdfdata) + 
  geom_line(mapping = aes(x = xt, y = nd), col = "red") + 
  geom_line(mapping = aes(x = xt, y = td), col = "blue") + 
  labs(x = "x", y = "Density")

# iii
huber.loss <- function(beta){
  # input vector of coefficient
  # return the sum of psi applied to residuals
  r <- y - x %*% beta
  c <- 1
  return(sum(ifelse(r^2 > c^2, 2*c*abs(r) - c^2, r^2)))
}

# iv
# first let's introduce function which applies gradient descent 
library(numDeriv)
grad.descent <- function(f, x0, max.iter = 200, step.size = 0.05, stopping.deriv = 0.01, ...) {
  # f: function need to be minimize
  # x0: initial point
  # max.iter
  # setp.size: eta
  # stopping.derive
  n    <- length(x0)
  # store every parameters in iteration
  xmat <- matrix(0, nrow = n, ncol = max.iter)
  xmat[,1] <- x0
  
  for (k in 2:max.iter) {
    # Calculate the gradient
    grad.cur <- grad(f, xmat[ ,k-1], ...) 
    
    # Should we stop?
    if (all(abs(grad.cur) < stopping.deriv)) {
      k <- k-1; break
    }
    
    # Move in the opposite direction of the grad
    xmat[ ,k] <- xmat[ ,k-1] - step.size * grad.cur
  }
  
  xmat <- xmat[ ,1:k] # Trim
  return(list(x = xmat[,k], xmat = xmat, k = k))
}
gd <- grad.descent(huber.loss, x0 = rep(0, p), step.size = 0.001, stopping.deriv = 0.1)
# the number of iteration
num_iter <- gd$k; num_iter
# final coefficient
final_coef <- gd$x; final_coef

# v
obj <- rep(NA, num_iter)
for (i in 1:num_iter){
  obj[i] <- huber.loss(gd$xmat[,i])
}
ggplot() + 
  geom_line(mapping = aes(x = 1:num_iter, y = obj)) + 
  labs(x = "Number of iteration", y = "Value of huber.loss()")

# vi
gd2 <- grad.descent(huber.loss, x0 = rep(0, p), step.size = 0.1, stopping.deriv = 0.1)
obj2 <- rep(NA, 50)
for (i in 1:50){
  obj2[i] <- huber.loss(gd2$xmat[,i+49])
}
ggplot() + 
  geom_line(mapping = aes(x = (gd2$k-49):gd2$k, y = obj2)) + 
  labs(x = "Number of iteration", y = "Value of huber.loss()")

# vii
sparse.grad.descent <- function(f, x0, max.iter = 200, step.size = 0.05, stopping.deriv = 0.01, ...) {
  # f: function need to be minimize
  # x0: initial point
  # max.iter
  # setp.size: eta
  # stopping.derive
  n    <- length(x0)
  # store every parameters in iteration
  xmat <- matrix(0, nrow = n, ncol = max.iter)
  xmat[,1] <- x0
  
  for (k in 2:max.iter) {
    # Calculate the gradient
    grad.cur <- grad(f, xmat[ ,k-1], ...) 
    
    # Should we stop?
    if (all(abs(grad.cur) < stopping.deriv)) {
      k <- k-1; break
    }
    
    # Move in the opposite direction of the grad
    xmat[ ,k] <- xmat[ ,k-1] - step.size * grad.cur
    xmat[, k][abs(xmat[, k]) < 0.05] <- 0
  }
  
  xmat <- xmat[ ,1:k] # Trim
  return(list(x = xmat[,k], xmat = xmat, k = k))
}
gd.sparse <- sparse.grad.descent(huber.loss, x0 = rep(0, p), step.size = 0.001, stopping.deriv = 0.1)
# final coefficient estimates
gd.sparse$x

# viii
regdata <- as.data.frame(cbind(y, x))
colnames(regdata) <- c("y", paste("x", "1":"10", sep = ""))
# estimates in the usual manner using lm()
b_lm <- lm(y ~.-1, data = regdata)$coefficient; b_lm
# compute the mse of using lm()
mse_lm <- mean((b_lm - b)^2); mse_lm
# compute the mse of question 4
mse_q4 <- mean((gd$x - b)^2); mse_q4
# compute the mse of question 7
mse_q7 <- mean((gd.sparse$x - b)^2); mse_q7

# ix
set.seed(10)
y <- x %*% b + rt(n, df = 2)
# in question 4
gd3 <- grad.descent(huber.loss, x0 = rep(0, p), 
                    step.size = 0.001, stopping.deriv = 0.1)
gd3$x
mse_gd <- mean((gd3$x - b)^2); mse_gd
# in question 7
gd.sparse2 <- sparse.grad.descent(huber.loss, x0 = rep(0, p), 
                                  step.size = 0.001, stopping.deriv = 0.1)
gd.sparse2$x
mse_sparsegd <- mean((gd.sparse2$x - b)^2); mse_sparsegd

# x
mse_gd2 <- rep(NA, 10)
mse_sparsegd2 <- rep(NA, 10)
for (i in 1:10){
  y <- x %*% b + rt(n, df = 2)
  gd4 <- grad.descent(huber.loss, x0 = rep(0, p), 
                      step.size = 0.001, stopping.deriv = 0.1)  
  mse_gd2[i] <- mean((gd4$x - b)^2)
  gd.sparse3 <- sparse.grad.descent(huber.loss, x0 = rep(0, p), 
                                    step.size = 0.001, stopping.deriv = 0.1)
  mse_sparsegd2[i] <- mean((gd.sparse3$x - b)^2)
}
# average
mean(mse_gd2)
mean(mse_sparsegd2)
# standard deviation
sd(mse_gd2)
sd(mse_sparsegd2)
# minimum
min(mse_gd2)
min(mse_sparsegd2)
