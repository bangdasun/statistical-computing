##############################
## Lecture 7 Student Script ##
##############################


## Section I: Random Number Generation ##

# R uses tricks that generate pseudorandom numbers that are indistinguishable from real random numbers
# pseudorandom generators produce a deterministic sequence that is indistinguishable from a true random sequence
# if you do not know hos it started

# how to distinguish non-randomness
#   if some values are missing, it's distinguishable
#   uniformity of distribution in the limit
#   autocorrelation is bad

# ture randomness
#   thermal detesion uses trailing decimal points on a thermometer
#   can use cosmic ray/radioactive decay arrival timings
#   Random.org

# Setting the seed
runif(10)
set.seed(10)
runif(10)
set.seed(10)
runif(10)

## The LCG
# xn = (a*xn-1 + c) mod m
# values between 0 and m-1, sequence repeat m times
# dividing by m gives the uniform distributed random numbers between 0 and 1


seed <- 10
# produce between 0 - 15
new.random <- function(a = 5, c = 12, m = 16) {
  out <- (a*seed + c) %% m
  # <<- allows to assign a new global variable in a local environment
  seed <<- out 
  return(out)
}
new.random()
new.random()
new.random()
new.random()

out.length <- 20
variants   <- rep(NA, out.length)
for (i in 1:out.length) {
  variants[i] <- new.random()
}
variants

# a little better way
# making c and m are relatively prime
variants   <- rep(NA, out.length)
for (i in 1:out.length) {
  variants[i] <- new.random(a = 131, c = 7, m = 16)
}
variants

variants   <- rep(NA, out.length)
for (i in 1:out.length) {
  variants[i] <- new.random(a = 129, c = 7, m = 16)
}
variants

variants   <- rep(NA, out.length)
for (i in 1:out.length) {
  variants[i] <- new.random(a=1664545, c=1013904223, 
                            m=2^32)
}
variants/2^(32)

?Random

## Section II: Simulating Random Variables ##

# d-pdf of pmf of distribution
# p-cdf of the distribution
# q-quantile of the distribution (inverse cdf)
# r-draws random numbers from distribution


# Built-in R functions
dnorm(0, mean = 0, sd = 1) 
1/sqrt(2*pi) 

x <- seq(-5, 5, by = .001)
plot(x, dnorm(x), main = "Standard Normal Density", pch = 2)

# P(Z < 0)
pnorm(0)

# P(-1.96 < Z < 1.96)
pnorm(1.96) - pnorm(-1.96)

# P (Z < ?) = 0.5
qnorm(.5) 

# P(Z < ?) = 0.975
qnorm(.975)

rnorm(1)
rnorm(5)
rnorm(10, mean = 100, sd = 1)

t <- seq(0, 2, length = 500)
e1 <- dexp(t, rate = 1)
e2 <- dexp(t, rate = 2)
e3 <- dexp(t, rate = 5)
library(ggplot2)
ggplot() + geom_line(mapping = aes(x = t, y = e1)) + 
  geom_line(mapping = aes(x = t, y = e2)) + 
  geom_line(mapping = aes(x = t, y = e3))

x <- 0:50
g1 <- dgeom(x, prob = .4)
ggplot() + 
  geom_line(aes(x = x, y = g1))
r1 <- rgeom(500, prob = .4)
hist(r1, breaks = 10)


# Student's t example
t  <- seq(-10, 10, by = .01)
df <- c(1, 2, 5, 30, 100)

plot(t, dnorm(t), lty = 1, col = "red", ylab = "f(t)", 
     main = "Student's t")

for (i in 1:5) {
  lines(t, dt(t, df = df[i]), lty = i) 
}

legend <- c(paste("df=", df, sep = ""), "N(0,1)")
legend("topright", legend = legend, lty = c(1:5, 1), 
       col = c(rep(1, 5), 2))

## task
# question 1
1 - pgamma(2, shape = 2, scale = 1)

# question 2: still with some bug...
t <- seq(0, 11, length = 100)
plot(t, dgamma(t, shape = 2, scale = 1), type = 'l')
for (i in 3:6){
  lines(t, dgamma(t, shape = i, scale = 1), col = i)
}
legend <- c(paste("shape = ", i, sep = ""))
legend("topright", legend = legend, fill = 1:5)

## correct
alpha <- 2:6
beta <- 1
x <- seq(0, 10, by = .01)
plot(x, dgamma(x, shape = alpha[1], rate = beta), col = 1, type = "l", ylab = "f(x)", main = "Gamma(alpha,1)")
for (i in 2:5){
  lines(x, dgamma(x, shape = alpha[i], rate = beta), col = i)
}
legend <- paste("alpha = ", alpha, sep = "")
legend("topright", legend = legend, fill = 1:4)

## task
# question 1
pnorm((190 + .5 - 1000 * .2) / sqrt(1000 * .2 * .8))

# question 2
pbinom(190, size = 1000, prob = 0.2)

# question 3
x <- rbinom(n = 500, size = 1000, prob = 0.2)
hist(x)

# The sample function
n <- 1000; p <- c(0.1, 0.2, 0.7)
x <- sample(1:3, size = n, prob = p, replace = TRUE)
head(x, 10)
rbind(p, p.hat = table(x)/n)

## task
# question 1
n <- 10000; p <- rep(1/6, length = 6)
x <- sample(1:6, size = n, prob = p, replace = TRUE)
rbind(p, p.hat = table(x)/n)

# question 2
x <- floor(runif(n, min = 0, max = 6))
rbind(p, p.hat = table(x)/n)

# The Inverse Transform Method
lambda <- 2
n      <- 1000
u      <- runif(n)

Finverse <- function(u, lambda) {
  return(ifelse((u<0|u>1), 0, -(1/lambda)*log(1-u)))
}
x <- Finverse(u, lambda)
hist(x, freq = FALSE, breaks = 15)
y <- seq(0, 10, .01)
lines(y, lambda*exp(-lambda*y), col = "blue")

## task
# question 1
# F = x^3, 0 <= x <= 1
# question 2
# x = F^(-1) = (F)^(1/3), 0 <= F <= 1
n <- 1000
u <- runif(n)
Finverse1 <- function(u){
  return(ifelse((u < 0|u > 1), 0, (u)^(1/3)))
}
x <- Finverse1(u)
hist(x, prob = TRUE)
y <- seq(0, 1, .01)
lines(y, 3*y^2)  
  


# Rejection Method
# what can we if all we've got is the pdf?
# by sampling candidates from an easier distribution then correcting the sampling probability by 
# randomly rejecting some candidates

plot(c(0,1), c(0,3), ty="n", main="A Sample Distribution", ylab="Density f(x)", xlab="x")
curve (dbeta(x, 3, 6), add=TRUE)
lines(c(0,0,1,1), c(0,3,3,0))
x1 <- runif(3, 0, 1); y1 <- runif(3, 0, 2.6)
selected <- y1 < dbeta(x1, 3, 6)

plot(c(0,1), c(0,3), ty="n", main="A Sample Distribution", 
     ylab="Density f(x)", xlab="x")
curve (dbeta(x, 3, 6), add=TRUE)
lines(c(0,0,1,1), c(0,3,3,0))
points(x1, y1, col=1+1*selected, cex=2)

mean(selected)
accepted.points <- x1[selected]

mean(accepted.points < 0.5) 
pbeta(0.5, 3, 6)

x2       <- runif(100000, 0, 1)
y2       <- runif(100000, 0, 10)
selected <- y2 < dbeta(x2, 3, 6)
mean(selected)

plot(c(0,1), c(0,6), ty="n", main="A Sample Distribution", 
     ylab="Density f(x)", xlab="x")
curve (dbeta(x, 3, 6), add=TRUE)
lines(c(0,0,1,1), c(0,3,3,0))
points (x2, y2, col=1+1*selected, cex=0.1)


# Acceptance-Rejection Algorithm
# we'd like to sample from a pdf:f
# suppose we know how to sample from a pdf g and we can easily calcuate g(x)
# let e() denote an envelope, with property
#   e(x) = g(x)/alpha >= f(x), alpha < 1
# sample Y ~ g and U ~ Unif[0,1], if U < f(y)/e(y), accept Y

# 1. sample Y ~ g
# 2. sample U ~ Unif[0,1]
# 3. if U < f(y)/e(y), accept Y. Set X = Y and consider X to be an element of the target random sample
#   equivalent to sampling U|y ~ U[0, e(y)] and keeping the value if U < f(y)


# good envelopes
# 1. envelope exceeds the target everywhere e(x) > f(x)
# 2. easy to sample from g
# 3. generate few rejected draws

# a simple approach: determine max f(x), then use a uniform distribution as g, and alpha = 1/[max f(x)]

f <- function(x) {
  return(ifelse((x < 0 | x > 1), 0, 60*x^3*(1-x)^2))
}

x <- seq(0, 1, length = 100)
plot(x, f(x), type="l", ylab="f(x)")

xmax  <- 0.6
f.max <- 60*xmax^3*(1-xmax)^2

e <- function(x) {
  return(ifelse((x < 0 | x > 1), Inf, f.max))
}
lines(c(0, 0), c(0, e(0)), lty = 1)
lines(c(1, 1), c(0, e(1)), lty = 1)
lines(x, e(x), lty = 1)

n.samps <- 1000             # number of samples desired
n       <- 0  	            # counter for number samples accepted
samps   <- numeric(n.samps) # initialize the vector of output
while (n < n.samps) {
  y <- runif(1)             # random draw from g
  u <- runif(1)
  if (u < f(y)/e(y)) {
    n        <- n + 1
    samps[n] <- y
  }
}
x <- seq(0, 1, length = 100)
hist(samps, prob = T, ylab = "f(x)", xlab = "x",
     main = "Histogram of draws from Beta(4,3)")
lines(x, dbeta(x, 4, 3), lty = 2)

## Section II: Monte Carlo Integration ##

# LLN
# if X1, X2, ..., Xn are iid with pdf p
# sum g(xi) / n --> int g(x)p(x)dx = Ep[g(X)]
# Monte Carlo principle
# to estimate int g(x)dx, draw from p(x) (p.d.f) and take the sample mean of f(x) = g(x)/p(x)


set.seed(1)
g.over.p <- function(x) {  
  return(sqrt(2*pi) * x^2 * exp(-(1/2)*x^2))
}
mean(g.over.p(rnorm(10000)))
sqrt(pi)/2

set.seed(1)

g <- function(x) {sqrt(pi)*x^2}
mean(g(rnorm(10000, sd = 1/sqrt(2))))
sqrt(pi)/2

# Monte Carlo approximation is unbasied
# the root mean square error is proportional to n^(-1/2)

# how to choose p?
# easy to estimate
# have low variance
# takes a simple form


## task
# question 1
g.over.p <- function(n){
  return(rexp(n, rate = 1/3) < 3)
}
mean(g.over.p(100000))

# question 2
pexp(3, rate = 1/3)

## check yourself
# question 1
x <- rbinom(5000, size = 16, prob = .5)

# question 2
mean(x)
sd(x)

# question 3
hist(x, prob = TRUE, breaks = 100)

# question 4
y <- rbinom(5e6, size = 1e6, prob = .5)
z <- (y - mean(y)) / (sd(y))
hist(z, prob = TRUE, breaks = 100)

##############################################################################################################
# pratice acceptance - rejection algorithm
# ex. fx = 60x^3(1-x)^2
f <- function(x){
  return(60*x^3*(1-x)^2)
}
curve(f, from = 0, to = 1)
f.max <- f(.6)
e <- function(x){
  return(f.max)
}
# we need 1000 draws
sample_size <- 100000
sample <- c()
# account the accept sample
accept <- 0

while (accept < sample_size){
  u <- runif(1)
  x <- runif(1)
  if (u < f(x) / e(x)){
    accept <- accept + 1
    sample <- c(sample, x)
  }
}
hist(sample, probability = TRUE, breaks = 100, ylim = c(0, f.max))
curve(f, from = 0, to = 1, add = TRUE)

