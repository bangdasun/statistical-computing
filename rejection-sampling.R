# Rejection sampling
# @author: Bangda Sun
# @date: 05/18/2017

# Generating random sample from a pdf f is easy if:
#
#   (1) it's a standard distribution (use build-in function)
#   (2) we have a nice, invertible cdf (inverse transformation)
#   (3) discrete (use sample() function in R)
# 
# Otherwise we can use rejection sampling
# NOTE: those case is mostly in one-dimensional case, for high-dimension,
#       more complicate methods will be used 

# Suppose we want to sample from a pdf f, and we also know how to 
# sample from another pdf g, where g(x) is easily to evaluate. 
# Then we introduce a function called envelope, with
#
#     e(x) = g(x) / alpha >= f(x)
#
# Sample Y ~ g(x) and U ~ Unif(0, 1):
# 
#     U <  f(Y) / e(Y) -> accepte Y
#     U >= f(Y) / e(Y) -> reject Y
# 
# In practice, g(x) is usually a uniform distribution, and alpha = 1 / max{f(x)}
# therefore e(x) = max{f(x)}, it's a constant function

### Algorithm ###
# 1. Set sample size N
# 2. Find max{f(x)}
# 3. While (number of accept < N)
#       U ~ Unif(0, 1)
#       Y ~ Unif(0, 1)
#       if (U < f(Y) / e(Y))
#           number of accept++
#           Y as a sample


### Example 1 ###
# Generate random sample from f(x) = 60*x^(3)(1-x)^(2), 0 <= x <= 1
f = function(x) {
  u = 60 * x^3 * (1-x)^2
  return(u)
}

f_max = f(0.6)

envelope = function(x) {
  u = ifelse((x < 0 | x > 1), Inf, f_max)
}

sample_size   = 100000
accept_size   = 0
random_sample = vector(mode = "numeric", length = sample_size)
while(accept_size < sample_size) {
  u = runif(1)
  y = runif(1)
  if (u < f(y) / envelope(y)) {
    accept_size = accept_size + 1
    random_sample[accept_size] = y
  }
}

t = seq(0, 1, by = .01)
ft = f(t)

library(ggplot2)
ggplot() + 
  geom_histogram(aes(random_sample, ..density..), binwidth = .01, fill = "lightblue", color = "black") +
  geom_line(mapping = aes(x = t, y = ft), size = 1)
