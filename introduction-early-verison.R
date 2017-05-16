# An Introduction to R

# 1 Introduction and preliminaries ######################################################
# 1.7 Getting help with functions and features
?help
?solve
??solve
help(solve)
help("[[")
help("if")
help.start()
example(solve)

# 1.11 Data permanency and removing objects
ls()
objects()
rm(a, b, c)

# 2. Simple manipulations; numbers and vectors ##########################################
# 2.3 Generating regular sequences
x <- c(1:20); x
y <- seq(-5, 5, by = .2); y
z <- seq(-5, 5, length = 50); z
a <- rep(6, 3); a

# 2.5 Missing values
#  NA: Not availiable
#  NaN: Not a number
z <- c(1:3, NA, 6:9); z
is.na(z)
sum(is.na(z))

0/0
Inf - Inf
x <- c(0:9, NaN, 10:13); x
is.nan(x)
sum(is.nan(x))

b <- c(); b

# 2.6 Character vectors
a <- c('X'); a
b <- c("X"); b
cat("Columbia University","\nin the city of New York")
labs <- paste(c("X", "Y", "Z"), 1:12, sep = ""); labs
paste(c("X", "Y",'Z'), collapse = "~~")
ss <- "|"
paste(c('X', 'Y', 'Z'), collapse = ss)
?Quotes

# 2.7 Index vectors; selecting and modifying subsets of a data set
#   1. logical vector
z <- x[!is.nan(x) & x >= 5]; z
z <- (x + 1)[!is.nan(x) & x > 0]; z

#   2. vector of positive integral quantities
x[1:10]

#   3. vector of negative integral quantities
x[-(1:4)]

#   4. vector of character strings
fruit <- c(5, 10, 1, 20); fruit
names(fruit) <- c("orange", "apple", "banana", "peach")
lunch <- fruit[c("apple")]; lunch

#   re-assign
x[is.nan(x)] <- 0; x
y[y < 0] <- -y[y < 0]; y

# 3. Objects, their modes and attributes ################################################
# 3.1 Intrinsic attributes: mode and length
# R operates on 'objects', examples are vectors of numeric or complex values, vectors of 
# logical. These are known as 'atomic' structures since their components are all of the 
# same type, or 'mode', namely 'numeric'(integer & double), 'complex', 'logical', 'character'
# 'raw'.

# vector must have values all of the same mode
x <- c("X", 1:20); x  # it will comes out character
x <- c(T, F, 1:20); x # TRUE and FALSE will change to 1 and 0
x <- NA; mode(x)      # the mode of NA is character
x <- NaN; mode(x)     # the mode of NaN is numeric

# list: mode of list, there are ordered sequences of objects which idividually can be of and modes
# list are known as 'recursive' rather than 'atomic' structures: since their components can
# themselves be lists in their own right

# other 'recursive' structures are those of mode 'function' and 'expression'

# length(), mode() can find out any defined structure

z <- 0:9; z
digits <- as.character(z); digits
d <- as.integer(digits); d
typeof(d)

# 3.2 Changing the length of an object
e <- numeric(); e
e[3] <- 17; e
length(e) <- 2; e

# 3.3 Getting and setting attributes (rare but sometimes very important)
attr(z, "dim") <- c(10, 10)

# 3.4 The class of an object
# all objects in R have a 'class', reported by the function class(). For simple vector it 
# just the mode, but 'matrix', 'array', 'factor', 'data.frame' are other possible values
winter
unclass(winter) # print as ordinary list

# 4. Ordered and unordered factors #####################################################
# factor is a vector objects used to specify a discrete classification (grouping)

# 4.1 A specific example
state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa",
           "qld", "vic", "nsw", "vic", "qld", "qld", "sa", "tas",
           "sa", "nt", "wa", "vic", "qld", "nsw", "nsw", "wa",
           "sa", "act", "nsw", "vic", "vic", "act")
statef <- factor(state)
statef
levels(statef)

# 4.2 The function of tapply() and ragged arrays ** important
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56,
            61, 61, 61, 58, 51, 48, 65, 49, 49, 41, 48, 52, 46,
            59, 46, 58, 43)
incmeans <- tapply(incomes, statef, mean); incmeans

# tapply() is a function applay a function to each group
# use self-defined function
stderr <- function(x){
  sqrt(var(x)/length(x))
}
incster <- tapply(incomes, statef, stderr); incster

# 4.3 Ordered factors
# ordered()

# 5. Arrays and matrices ###############################################################
# 5.1 Arrays
# arrays can be considered as a multiply subscripted collection of data entries
rm(z)
z <- array(1:100, dim = c(2, 5, 10))
dim(z) 

# 5.2 Array indexing, subsection of an array
z[2, 1, 1]

# 5.3 Index matrices
x <- array(1:20, dim = c(4, 5)); x
i <- array(c(1:3, 3:1), dim = c(3, 2)); i
x[i]
x[i] <- 0; x

# negative indices are not allowed in index matrices. NA and zero values are allowed:
# rows in the index matrix containing a zero are ignored, and rows containing an NA
# produce an NA in the result
# example **
Xb <- matrix(0, n ,b)
Xv <- matrix(0, n, v)
ib <- cbind(1:n, blocks)
iv <- cbind(1:n, varieties)
Xb[ib] <- 1
Xv[iv] <- 1
X <- cbind(Xb, Xv)

# construct incidence matrix
N <- crossprod(Xb, Xv)
# simpler way
N <- table(blocks, varieties)

x <- c(1:5); y <- c(1:5)
crossprod(x, y)

# 5.5 Outer product of two arrays
a <- c(1:3); b <- c(6:8)
ab <- a %o% b; ab

# alternative way
ab <- outer(a, b, '*'); ab

# '*' can be replaced
f <- function(x, y) {
  cos(y)/(1 + x^2)
}
z <- outer(a, b, f); z

# an example: determinant of 2*2 single digit matrices

d <- outer(0:9, 0:9); d
fr <- table(outer(d, d, "-")); fr
plot(as.numeric(names(fr)), fr, type = "h", 
     xlab = "Determinant", ylab = "frequency")

# 5.6 Generalized transpose of an array
A <- matrix(c(0,8), ncol = 2); A
B <- aperm(A, c(2,1)); B
B <- t(A); B

# 5.7 Matrix facilities
# matrix is just an array with two subscripts
# 5.7.1 matrix multiplication
A %*% B
A * t(B)
rm(x); x <- c(1, 2)
x %*% A %*% x # quadratic form
# crossprod: x^{T}x
# %o%: xx^{T}

diag(10) # identity matrix

# 5.7.2 linear equations and inversion
b <- A %*% x
solve(A, b)# solve the linear system
solve(A)   # find the inverse
# x^{T}A^{-1}x: x %*% solve(A, x), rather than compute the inverse of A

# 5.7.4 singular value decomposition and determinants *
svd(M) # M = U %*% D %*% t(V)

# 5.7.5 least square and QR decomposition
lsfit()
qr()

# 5.8 Formating partitioned matrices, cbind() and rbind()
A <- rbind(A, A); A
A <- cbind(A, A); A

# 5.9 The concatenation function, c(), with arrays
a <- c(A); a
b <- as.vector(A); b

# 5.10 Frequency tables from factors
statefr <- table(statef); statefr
statefr <- tapply(statef, statef, length); statefr
factor(cut(incomes, breaks = 35 + 10 * (0:7))) -> incomef
table(incomef, statef)

# 6. Lists and data frames ###############################################################
# 6.1 Lists
# list is an object consisting of an ordered collection of objects known as its components
Lst <- list(name = "Fred", wife = "Mary", no.children = 3, child.ages = c(4, 7, 9)); Lst
Lst$name
Lst$child.ages
x <- "name"; Lst[[x]]

# 6.2 Constructing and modifying lists
Mat <- A
Lst[5] <- list(matrix = Mat)
Lst

# concatenating lists
list.ABC <- c(list.A, list.B, list.C)

# 6.3 Data frames
# the components must be vectors, factors, numeric matrices, lists
accountants <- data.frame(home = statef, loot = incomes, shot = incomef)
accountants
plot(accountants)

# attach(), detach()

# 7. Reading data from files #############################################################

# 7.1 read.table()

# 7.2 scan()

# 7.3 data()

# 7.4 edit()

# 9. Grouping, loops and conditional execution ##########################################


# 10. Writing your own functions ########################################################
# 10.2 define new binary operators

"%!%" <- function(x, y){
  # return the sum of square of two vectors
  ss <- sum(sqrt(x^2 + y^2))
  return(ss)
}
x <- c(1:10)
y <- x
x%!%y

# 10.5 assginments within functions
# if global and permanent assignments are intended within a function, then either <<- or assign()
# 