# Inverse Transformation
# @author: Bangda Sun
# @date: 05/19/2017

# Suppose we know X ~ F (cdf), we can generate U ~ Unif(0,1)
# then evaluate F^{-1}(U), these would be the sample of X
# Therefore:
#
#   (1) F should invertible, 
#   (2) X is continuous
#
#


### Algorithm ###
# 1. Find F^{-1}, F is the cdf
# 2. For n = 1 to N
#       u ~ Unif(0, 1)
#       x = F^{-1}(u)

### Example 1 ###
# Suppose we want to sample from f(x) = 2 * exp(-2*x)

forigin = function(x) {
  u = 2 * exp(-2*x)
  return(u)
}

Forigin = function(x) {
  u = 1 - exp(-2*x)
  return(u)
}

Finverse = function(u) {
  x = ifelse((u < 0) | (u > 1), 0, -(1/2) * log(1 - u))
  return(x)
}

u  = runif(10000, min = 0, max = 1)
x  = Finverse(u)
t  = seq(.001, 3.1, by = .1) 
ft = forigin(t)

library(ggplot2)
ggplot() +
  geom_histogram(aes(x, ..density..), bins = 100, fill = "lightblue", color = "black", 
                 stat = "bin") +
  geom_line(mapping = aes(x = t, y = ft), size = 1) +
  xlim(0, 3.1)


### Example 2 ###
# Suppose we want to sample from f(x) = 3*x^2, 0 < x < 1

forigin = function(x) {
  u = 3 * x^2
  return(u)
}

Forigin = function(x) {
  u = x^3
  return(u)
}

Finverse = function(u) {
  x = u^(1/3)
  return(x)
}

u  = runif(10000)
x  = Finverse(u)
t  = seq(.01, 1.1, by = .1) 
ft = forigin(t)

ggplot() +
  geom_histogram(aes(x, ..density..), binwidth = .1, fill = "lightblue", color = "black", 
                 stat = "bin") +
  geom_line(mapping = aes(x = t, y = ft), size = 1) +
  xlim(0, 1)
