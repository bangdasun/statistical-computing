######################
### Variable types ###
######################

## Assign variables
x = 2
y = "columbia"
z = TRUE

## Variable type
c(str(x), mode(x), typeof(x), class(x))
c(str(y), mode(y), typeof(y), class(y))
c(str(z), mode(z), typeof(z), class(z))

## Basic data type and data structure
### Vector
# Data type of all elements should be same, if different, will convert to character
vec    = c(1, 1, 2, 3, 5, 8, NA, NULL)
ariseq = seq(4)
idvec  = rep(TRUE, 5)
auto   = vector(mode = "numeric", length = 10)

vec[c(1, 4)]
vec[vec > 2]
which(vec > 2)
names(vec)
range(vec, na.rm = TRUE)
quantile(vec)

### Matrix
# 2-D array, all elements should have same data type
mat    = matrix(c(.1, .5, .4, .3, .2, .5, .4, .1, .5), byrow = TRUE, ncol = 3)
A      = matrix(c(1, 2, 3), ncol = 1)
B      = matrix(c(3, 2, 3, 4, 5, 7), ncol = 2)

mat[1, 3]
mat[2, ]
which(mat > .2)
cbind(A, B)
rbind(t(A), t(B))
colnames(A)
rownames(B)
dim(mat)
diag(mat)
solve(mat) # two useages
det(mat)
mat %*% A

### Array
# multi-dimension
arr    = array(c(1:12), dim = c(3, 2, 2))
dim(arr)

### List
# Like vector but elements can have different data type
lst    = list(num = x, char = y, logi = z)

lst[[1]] # Return original data type
lst[1]   # Return list
lst$num
lst["num"]
unlist(lst)

### Dataframe
# Like matrix but elements can have different data type (column), basic data type for analysis in R
df     = data.frame(course = c("STAT 5203", "STAT 5204", "STAT 5205", "STAT 5206", "STAT 5207", "STAT 5241"),
                    grade  = c("A-", "A", "A", "A+", "A", "A+"),
                    hwnum  = c(6, 4, 7, 10, 4, 6))

df$course
df["course"]
df[3, ]
df[df$hwnum > 4, "course"]
df$semester = c("16fall", "16fall", "16fall", "16fall", "17spring", "17spring") # Only in dataframe
summary(df)
head(df)
tail(df)

## Logic and Flow Control
### Filtering: logical operators !, &, |; relational operators <=, <, >=, >, ==, !=
(5 > 4) | (4 > 3)
(5 != 1) && (4 == 3)

### Flow control
if ((1 < 2) && (3 < 4)) {
  print("TURE")
} else {                   # else if
  print("FALSE")
}

# Example: sum 1 to 100
i   = 1
sum = 0
while (i <= 100) {
  sum = sum + i
  i   = i + 1
}
sum

sum = 0
for (j in 1:100) {
  sum = sum + j
}
sum

break
next
ifelse # Useful function

## File I/O
# Import Data
getwd()
setwd(path)
read.table(file, sep =  )
read.csv(file, header = , as.is = )
scan

# Exporting Data
write.table(object, file = "")
write.csv(object, file = "")

## Data Manipulation
### Clean Data
# is the first row a header? what about the first column
# symbols: ?, %, &, *, etc can do funny things
# headers, footers, side comments, notes will mess up the structure
# missing value sometimes are denoted as 999 or N.A. (need converted)