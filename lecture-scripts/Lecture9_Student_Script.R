##############################
## Lecture 9 Student Script ##
##############################

## Section I: Functions as Objects ##

# What does a function do?

sample
log
rowSums

# Function classes

class(sin)
class(sample)
resample = function(x) { 
  return(sample(x, size=length(x), replace=TRUE)) 
}
class(resample)

# Facts About Functions

# see body code
body(resample)
# see arguments
args(resample)
environment(resample)

# The grad() function
library(numDeriv)
args(grad)

simpleFun = function(x) { 
  # x is a vector of length 2
  return(x[1]^2 + 1/3*x[2]^2)
}

xpt <- runif(n = 2, min = -2, max = 2)
# calculate the gradient
grad(simpleFun, xpt)
# check the gradient
c(2*xpt[1], 2/3*xpt[2])

## Section II: Basic Optimization ##
# in multivariate version
# grad(f(x0)) points in the direction of fastest ascent at x0

# single variable grad descent
f <- function(x){return(x^2)}
grad(f, 2)
theta <- 1
eta <- 0.01
stop.derive <- 0.0001
while (grad(f, theta) > 0){
  if (grad(f, theta) < stop.deriv){
    break
  }
  theta = theta - eta * grad(f, theta)
}
theta
f(theta)

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

x0 <- c(-1.9, -1.9)
gd <- grad.descent(simpleFun, x0)
gd$x
gd$k


n <- 100
p <- 2
pred <- matrix(rnorm(n*p), n, p)
beta <- c(1, 4)
resp <- pred %*% beta + rnorm(n)
lm.coefs <- coef(lm(resp ~ pred + 0))
lm.coefs

MSE <- function(beta) { 
  return(sum((resp - pred %*% beta)^2))
}
grad.descent(MSE, x0 = c(0,0), step.size = 0.05, max.iter = 200)
out = grad.descent(MSE, x0 = c(0,0), step.size = 1e-3, max.iter = 200) 
out$k
out$x
lm.coefs 

# GMP Example
setwd("C://Users//Bangda//Desktop//GR5206 Materials")
gmp     <- read.table("gmp.txt", header = TRUE)
gmp$pop <- gmp$gmp/gmp$pcgmp

# install.packages("numDeriv")
library(numDeriv)

mse <- function(theta) {
  mean((gmp$pcgmp - theta[1]*gmp$pop^theta[2])^2)
}

grad.mse <- function(theta) {grad(func = mse, x = theta)}
theta0   <- c(5000, 0.15)

fit1     <- optim(theta0, mse, grad.mse, method = "BFGS", hessian = TRUE)
fit1

fit2 <- nls(pcgmp ~ theta0*pop^theta1, data = gmp, start = list(theta0 = 5000, theta1 = 0.10))
summary(fit2)

plot(pcgmp ~ pop, data = gmp, log = 'x')
pop.order <- order(gmp$pop)
lines(gmp$pop[pop.order], fitted(fit2)[pop.order])
curve(fit1$par[1]*x^fit1$par[2], add = TRUE, lty = "dashed", col = "red")

## Section V: Constrained Optimization ##
# Linear Programming
plot(0,type="n",xlim=c(0,45),ylim=c(0,45),xlab="cars",ylab="trucks", cex.axis = 2, cex.lab = 2)
abline(70/3,-1/3,lty="dashed",lwd=4)
abline(80/3,-2/3, lty = "dotted",lwd=4)
legend("topright",legend=c("labor","steel"),
       lty=c("dotted","dashed"), lwd = c(4,4), cex = 2)
for (i in 1:30) {abline(i,-13/27,col="grey",lwd=3)}

factory.n <- list(c("labor","steel"), c("car","truck"))
factory   <- matrix(c(40, 1, 60, 3), nrow = 2, dimnames = factory.n)
available        <- c(1600, 70)
names(available) <- rownames(factory)
prices           <- c(car = 13, truck = 27)

revenue <- function(output) {return(-output %*% prices)}
plan <- constrOptim(theta = c(5, 5), f = revenue, grad = NULL, ui = -factory, 
                    ci = -available, meth = "Nelder-Mead")
plan$par