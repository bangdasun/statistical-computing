##############################
## Lecture 1 Student Script ##
##############################

## Section II: What is R? ##

5+4
5*4

mean(abs(rnorm(100)))

hist(rnorm(100))

# Create a vector in R named "x"
x <- c(5, 29, 13, 87)
x

x <- 1:50
x

## Section III: Variable Types, Vectors, and Matrices ##
# Modes: numeric, complex, character, logical
# Numeric
x <- 2
mode(x)
typeof(x)
y <- as.integer(3)
typeof(y)

# Complex
z <- 1 - 2i
z
typeof(z)

# Character
name <- "Columbia University"
name
typeof(name)

# Logical
a <- TRUE
b <- F
a
b
typeof(a)

# Test your understanding
3*TRUE # Logicals in arithmetic. It will consider True as 1 and False as 0!!
mode(3*TRUE)
mode("147")

# Defining a numeric vector
x <- c(2, pi, 1/2, 3^2)
x

# Defining a character vector
y <- c("NYC", "Boston", "Philadelphia")
y


# A sequential list of integers:
z <- 5:10
z

# Using rep() to create a 1's vector:
u <- rep(1, 18)
u

# *******************************
# Allocating space and filling in
v <- c()

v[1] <- TRUE
v[2] <- TRUE
v[3] <- FALSE

v

# Nesting c()
vec1 <- rep(-27, 3)
vec1

vec2 <- c(vec1, c(-26, -25, -24))
vec2

# Building a matrix that fills in by column:
mat <- matrix(1:9, nrow = 3, ncol = 3)
mat

# Building a matrix that fills in by row:
new_mat <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
new_mat

# Allocating the space for a matrix then filling it in:
this_mat <- matrix(nrow = 2, ncol = 2)

this_mat[1,1] <- sqrt(27)
this_mat[1,2] <- round(sqrt(27), 3)
this_mat[2,1] <- exp(1)
this_mat[2,2] <- log(1)

this_mat

# Using rbind()
vec1 <- rep(0, 4)
vec2 <- c("We're", "making", "matrices", "!")

final_mat <- rbind(vec1, vec2)
final_mat

# Column names
this_mat # Defined previously
colnames(this_mat) # Nothing there yet
colnames(this_mat) <- c("Column1", "Column2")
this_mat

# Mixing Modes
vec <- c(1.75, TRUE, "abc")
vec
str(vec) # internal structure of vector


# What does the str() function do?
?str # Function help

??"structure" # Fuzzy matching

# Subsetting vectors
y <- c(27, -34, 19, 7, 61)
y[2]
y[3:5]
y[c(1, 4)]
y[c(1, 4)] <- 0
y
y <- c(27, -34, 19, 7, 61)
y
y[-c(1, 4)]
y <- y[-1] # Drop the 1st element
y

# Subsetting matrices
mat <- matrix(1:10, ncol = 2)
mat
mat[, 1]

this_mat
this_mat[, "Column2"]
this_mat[, -1]

## Section IV: An Extended Example ##

# Installing the "pixmap" package.
install.packages("pixmap")
library("pixmap")

# This won't work unless you have downloaded casablanca.pgm and put it 
# in your working directory (which we'll learn about next week).
casablanca_pic <- read.pnm("casablanca.pgm")
casablanca_pic
plot(casablanca_pic) # locator(2)

dim(casablanca_pic@grey)
casablanca_pic@grey[360, 100]
casablanca_pic@grey[180, 10]

casablanca_pic@grey[15:70, 220:265] <- 1
plot(casablanca_pic)

# Check your understanding
z <- matrix(rep(1:9), nrow = 3)
colnames(z) <- c("First", "Second", "Third")
z

z[2:3, "Third"]
c(z[,-(2:3)], "abc")
rbind(z[1,], 1:3)

## Section V: More on Vectors and Matrices ##

# Example 1
x <- c(49.3,59.3,68.3,48.1,57.61,78.1,76.1)
y <- c(1894,2050,2353,1838,1948,2528,2568) 

n=length(x) # Sample size
n

max(x)
sd(x)

summary(x) # Summary statistics
summary(y)

# Vectorized Operations
u <- c(1,3,5)
v <- c(1,3,5)
v + 4 # Recycling
v + c(1,3) # Recycling
v + u
u <- c(1,3,5)
v <- c(1,3,5)

'+'(v,u)
'*'(v,u)

# Example 1 again
plot(x,y, xlab = "Body Mass", ylab = "Energy Expenditure")

dev_x <- x - mean(x) # First, compute x and y deviations 
dev_y <- y - mean(y)

Sxy <- sum(dev_x * dev_y) # Next, compute sum of squares of xy and xx
Sxx <- sum(dev_x * dev_x)
 
Sxy/Sxx # Compute the estimated slope

mean(y) - (Sxy/Sxx) * mean(x) # Compute the estimated intercept

# Example 2 
A <- matrix(c(3,1,1,-2,1/2,1,1,-12,3), nrow = 3) 
b <- c(-1, 2, 3) 

solve(A, b) 

x <- c(1, 2, 0) # Define solution vector x
A %*% x         # Then check with matrix multiplication

# Element-wise matrix operations

A <- matrix(c(1, -2, -2, 4), nrow = 2, byrow = TRUE)
 
identity <- diag(2) # Define a 2 by 2 identity matrix
identity

det(A - 5*identity) # Check if 5 is an eigenvalue of A

## Section VI: NULL and NA Values

length(c(-1, 0, NA, 5))
length(c(-1, 0, NULL, 5)) # NULL will not be count

t <- c(-1,0,NA,5) 
mean(t)
mean(t, na.rm = TRUE) # Use na.rm = TRUE to remove NA values

s <- c(-1, 0, NULL, 5) # NA values are missing, but NULL values don't exist.
mean(s)

# NULL can be used to pre-allocation space
x <- NULL

x[1] <- "Blue"
x[2] <- "Green"
x[3] <- "Red"
x

## Section VII: Filtering ##

# Relational Operators
1 > 3
1 == 3
1 != 3

# Logical Operators
(1 > 3) & (4*5 == 20)
(1 > 3) | (4*5 == 20)

# Some Basic Examples
c(0,1,4) < 3
which(c(0,1,4) < 3)
which(c(TRUE, TRUE, FALSE))

c(0,1,4) >= c(1,1,3)
c("Cat","Dog") == "Dog"

# Filtering a Vector
w <- c(-3, 20, 9, 2)

w[w > 3] # Extract elements of w greater than 3

w > 3
w[c(FALSE, TRUE, TRUE, FALSE)]

w <- c(-3, 20, 9, 2)
which(c(0, 1, 4) < 3)

w[w*w >= 3 & w*w <= 10] # Extract elements of w with squares between 3 and 10

w*w >= 3 # Output logical values
w*w <= 10
w*w >= 3 & w*w <= 10

w <- c(-1, 20, 9, 2)
v <- c(0, 17, 10, 1)

w[w > v] # Extract elements of w greater than elements from v

w > v
w[c(FALSE, TRUE, FALSE, TRUE)]

# Filtering matrices
M <- matrix(c(rep(4,5), 5:8), ncol=3, nrow=3)
M

M > 5
M[,3] < 8
M[M[,3] < 8, ]

M[M > 5] <- 0
M

# Check your understanding
z <- matrix(c(1:3, TRUE, FALSE, TRUE, 9, 16, 25), nrow = 3)
colnames(z) <- c("First", "Second", "Third")
z
z[z[, "Second"], ]
z[, 1] != 1
z[(z[, 1] != 1), 3]

z[(z[, 1] != 1), 3]
z[(z[, 1] != 1), 3, drop = FALSE]

## Section VIII: A Note on Lists ##

# Example
x <- c(49.3,59.3,68.3,48.1,57.61,78.1,76.1)
y <- c(1894,2050,2353,1838,1948,2528,2568) 

data <- cbind(x, y) # Combine data into single matrix

sum_x <- summary(x) # Summary values for x and y
sum_y <- summary(y)

est_vals <- c(Sxy/Sxx, mean(y) - Sxy/Sxx*mean(x)) # We computed Sxy and Sxx previously 

body_fat <- list(variable_data = data, 
                 summary_x = sum_x, summary_y = sum_y, 
                 LOBF_est = est_vals)

body_fat[[1]]
body_fat$LOBF_est
body_fat[["summary_x"]]


##########################################################################
# examples from reading material

