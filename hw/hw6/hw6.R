# i
f <- function(x, a = 2.654, x.min = 407760){
  # p.d.f of Pareto distribution
  f <- (a - 1)/x.min * (x / x.min)^(-a)
  # the support of x is [xmin, infty)
  return(ifelse((x < x.min), 0, f))
}
x <- 407760:1000000
plot(x, f(x), xlab = "x", ylab = "f(x)", type = 'l')

# ii
upper.income <- function(u, a = 2.654, x.min = 407760){
  # calculate and return F inverse
  Finv <- x.min * (1 - u)^(1/(-a+1))
  # the support of u is (0, 1)
  return(ifelse((u < 0|u > 1), 0, Finv))
}
upper.income(.5)

# iii
u <- runif(1000)
X <- upper.income(u)
hist(X, breaks = 50, probability = TRUE, xlab = "x", ylab = "f(x)",
     main = "Histogram of draws from f(x) and the actual f(x)")
curve(f, min(X), max(X), add = TRUE, col = "red")

# iv
# median of the simulate set
median(X)
# 50th percentile of the Pareto
upper.income(.5)

# v
f <- function(x){
  return(ifelse((x < -1|x > 2), 0, (4-x^2)/9))
}
x <- seq(-3, 3, length = 5000)
plot(x, f(x), xlab = "x", ylab = "f(x)", type = 'p', cex = .05)

# vi
f.max <- 4/9
e <- function(x){
  return(ifelse((x < -1|x > 2), Inf, f.max))
}

# vii
# number of sampled needed
n        <- 1000
# count the accepted sample
n.accept <- 0
# the simulated sample
sample   <- numeric(n)
while (n.accept <= n){
  u <- runif(1)
  # sample from g(x)
  y <- runif(1, -1, 2)
  # accept condition
  if (u < f(y)/e(y)){
    n.accept <- n.accept + 1
    sample[n.accept] <- y
  }
}
head(sample)

# viii
hist(sample, probability = TRUE, xlab = "x", ylab = "f(x)",
     main = "Histogram of draws from f(x)")
curve(f, -1, 2, add = TRUE, col = "red")

# ix
x <- 5
x.vals <- c()
while (x > 0){
  r <- runif(1, -2, 1)
  x.vals <- c(x.vals, x)
  x <- x + r
}
print(x.vals)

# x
plot(0:(length(x.vals) - 1), x.vals, xlab = "Iteration Number", ylab = "x", type = "o")

# xi
random.walk <- function(x.start = 5, plot.walk = TRUE){
  x <- x.start
  x.vals <- c()
  while (x > 0){
    r <- runif(1, -2, 1)
    x.vals <- c(x.vals, x)
    x <- x + r
  }
  if (plot.walk == TRUE){
    plot(0:(length(x.vals) - 1), x.vals, xlab = "Iteration Number", ylab = "x",
         type = "o")
  }
  return(list(x.vals = x.vals, num.steps = length(x.vals) - 1))
}
random.walk()
random.walk()
random.walk(x.start = 10, plot.walk = FALSE)
random.walk(x.start = 10, plot.walk = FALSE)

# xii
n <- 10000
iter.num <- rep(NA, n)
for (i in 1:n){
  iter.num[i] <- random.walk(plot.walk = FALSE)$num.steps
}
# expected iteration
mean(iter.num)

# xiii
random.walk <- function(x.start = 5, seed = NULL, plot.walk = TRUE){
  set.seed(seed)
  x <- x.start
  x.vals <- c()
  while (x > 0){
    r <- runif(1, -2, 1)
    x.vals <- c(x.vals, x)
    x <- x + r
  }
  if (plot.walk == TRUE){
    plot(0:(length(x.vals) - 1), x.vals, xlab = "Iteration Number", ylab = "x",
         type = "o")
  }
  return(list(x.vals = x.vals, num.steps = length(x.vals) - 1))
}
random.walk()
random.walk()
random.walk(seed = 33, plot.walk = FALSE)
random.walk(seed = 33, plot.walk = FALSE)
