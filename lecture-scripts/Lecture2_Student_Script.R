##############################
## Lecture 2 Student Script ##
##############################


## Section I: Filtering ##

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

w[w*w >= 3 & w*w <= 10] # Extract elements of w with squares between 3 and 10

w*w >= 3
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

## Section II: A Note on Lists ##

# Example
x <- c(49.3,59.3,68.3,48.1,57.61,78.1,76.1)
y <- c(1894,2050,2353,1838,1948,2528,2568) 

# First, compute x and y deviations 
dev_x <- x - mean(x)
dev_y <- y - mean(y)

# Next, compute sum of squares of xy and xx
Sxy <- sum(dev_x * dev_y)
Sxx <- sum(dev_x * dev_x)

data <- cbind(x, y) # Combine data into single matrix

sum_x <- summary(x) # Summary values for x and y
sum_y <- summary(y)

est_vals <- c(Sxy/Sxx, mean(y) - Sxy/Sxx*mean(x)) # We computed Sxy and Sxx previously 

body_fat <- list(variable_data = data, 
                 summary_x = sum_x, summary_y = sum_y, 
                 LOBF_est = est_vals)

# Return Lists
body_fat["summary_y"]
body_fat[c(4,2)]

#Return other objects
body_fat[[1]]
body_fat$LOBF_est
body_fat[["summary_x"]]

## Section III: A Note on Lists ##

# NA is missing, NULL is non-existant: NULL is useful in loops
length(c(-1, 0, NA, 5))
length(c(-1, 0, NULL, 5))

# Use na.rm = TRUE to remove NA values
t <- c(-1,0,NA,5)
mean(t)
mean(t, na.rm = TRUE)


# NA values are missing, but NULL values don't exist.
s <- c(-1, 0, NULL, 5)
mean(s)

# Define an empty vector
x <- NULL

# Fill in the vector
x[1] <- "Blue"
x[2] <- "Green"
x[3] <- "Red"
x


# Use of is.na()

is.na(c(3,-7.5, NaN, pi))
3/0
is.na(3/0)
0/0
is.na(0/0)
is.na(NA)
is.na(NaN)
is.nan(NaN)
is.nan(NA)

## Section IV: Factors and Tables ##

# Factors example

data <- rep(c("Control","Treatment"),c(3,4))
data # A character vector
group <- factor(data) # space ssving
group
str(group)
mode(group) # Numeric?
summary(group)
table(group, group)

# Split() Function

group
ages <- c(20, 30, 40, 35, 35, 35, 35)
sex <- c("M", "M", "F", "M", "F", "F", "F")
split(ages, list(group, sex))

# Table() Function: sth like, cross

group
table(group)
table(sex, group)
table(ages, group)


new_table <- table(sex, group)
new_table[, "Control"]
round(new_table/length(group), 3) # Gives proportions


## Section V: Dataframes ##

# Creating Dataframes
Name <- c("John", "Jill", "Jacob", "Jenny")
Year <- c(1,1,2,4)
Grade <- c("B", "A+", "B-", "A")
student_data <- data.frame(Name, Year, Grade, 
                           stringsAsFactors = FALSE)

student_data
dim(student_data)
str(student_data)
summary(student_data)

# States Example

library(datasets)
states <- data.frame(state.x77, Region = state.region, 
                     Abbr = state.abb)
head(states, 2)
student_data
student_data[3:4,]

# Accessing Dataframes

student_data
student_data$Grade
states["New York", ] # Can also use rownames

# Filtering Dataframes
student_data[student_data$Grade == "A+", ]
student_data[student_data$Year <= 2, ]
states[states$Region == "Northeast", "Population"]

# Adding Columns and Rows to Dataframes
new_stu <- c("Bobby", 3, "A")
student_data <- rbind(student_data, new_stu)
student_data
student_data$School <- "Columbia"
student_data
 # this construction would not work with a matrix

# Merging Dataframes
Name <- c("John", "Bobby")
Age <- c(29, 23)
student_data2 <- data.frame(Name, Age, stringsAsFactors = FALSE)
student_data2
student_data
student_data
merge(student_data, student_data2)

# Data as an argument to a function
plot(Illiteracy ~ Frost, data = states)


## Section VI: Importing Data into R ##

setwd("file_path")
getwd()
list.files()

HC <- scan("C:\\Users\\Bangda\\Desktop\\GR5206 Materials\\HonorCode.txt", what = "")
head(HC, 20)
str(HC)

# ?, %, &, * etc can make R do funny thing.
# is the first row a header? what about the first column
# how are missing values indicated? It should be NA but ofter something like 999 or N.A.
# headers, footers, side comments can mess up

# output: write.table(), write.csv()


## Section VII: Data in R: A Text Example##

# Text Example
HC <- scan("HonorCode.txt", what = "")
head(HC, 15) # HC is a vector and each word of the Honor Code is an element of the vector
HC <- factor(HC, levels = unique(HC))

# Functions:
#   have names
#   ususally take in argument
#   inclide body of code that does sth
#   usually return an object at the end
square_it <- function(x){
  out <- x*x
  return(out)
}

square_it(2); square_it(-4); square_it(146)

# Remember split()
split(ages, list(group, sex))


# The text Example: compiles a list of location of each occurance of each word in the text
findwords <- function(text_vec){
  words <- split(1:length(text_vec), text_vec)
  return(words)
  # length(textfile) is the total number of words in the textfile, in honor code it is 443
  # textfile is a factor, with each unique word as a level. there are 243 levels
  # split() than determines the location of each unique word and returns the locations in list form
}
findwords(HC)[1:3]

HC <- as.character(HC)
HC[c(1, 48, 142, 204, 232, 310, 331)] # students
HC[c(2, 206)] # should



alphabetized_list <- function(wordlist) {
  nms <- names(wordlist) # The names are the words
  sorted <- sort(nms) # The words, but now in ABC order
  return(wordlist[sorted]) # Returns the sorted version
}

wl <- findwords(HC)
alphabetized_list(wl)[1:3]

## Section VIII: Controls Statements: Loops, While, If Else ##

# For loops
x <- c(5, 12, -3)
for (i in x) {
  print(i^2)
}

for (i in seq(4)) {
  if (i %% 2 == 0){print(log(i))}
  else {print("Odd")}
}



# While loops
i <- 1 
while (i <= 10) i <- i + 4
i

# body of a for loop can contain other for loops called nesting or other control statements.



# If, else statements
for (i in seq(4)) {
  if (i %% 2 == 0) {print(log(i))}
  else {print("Odd")}
}

# Check your Understanding
library(matlab)    
total <- 0
for (i in 1:10) {
  if(isprime(i)) {
    total <- total + i
  }
}
total

## Section IX: Vectorized Operations ##

# ofter faster!

# Adding two vectors with a loop
u <- c(1,2,3)
v <- c(10,-20,30)
c <- vector(mode = "numeric", length = length(u))

for (i in 1:length(u)) {
  c[i] <- u[i] + v[i]
}
c

# Adding two vectors with a vectorized operation
c <- u + v
c

# Vectorized Conditionals
for (i in seq(4)) {
  if (i %% 2 == 0) {print(log(i))}
  else {print("Odd")}
}

ifelse(seq(4) %% 2 == 0, log(seq(4)), "Odd")

# The apply() Function
mat <- matrix(1:12, ncol = 6)
mat
colSums(mat) # Recall colSums() from lab.
colSums(mat) 
apply(mat, 2, sum)
apply(mat, 1, sum) # Calculates the row sums


# lappy() and sapply()
vec1 <- c(1.1,3.4,2.4,3.5)
vec2 <- c(1.1,3.4,2.4,10.8)
not_robust <- list(vec1, vec2)  
lapply(not_robust, mean)
lapply(not_robust, median) # return a list too 
sapply(not_robust, median) # return a vector
unlist(lapply(not_robust,median))

# Text Example
wl[1:3] # wl for word list
freq_list <- function(wordlist) {
  freqs <- sapply(wordlist, length) # The frequencies
  return(wordlist[order(freqs)])
}
head(freq_list(wl), 3)
tail(freq_list(wl), 3)

# tapply()
group
ages <- c(20, 30, 40, 35, 35, 35, 35) 
tapply(ages, group, mean)

mat <- matrix(1:12, ncol = 6)
colSums(mat)
apply(mat, 2, sum)
apply(mat, 1, sum)  # calculate the row

# lapply() or list apply, works like apply()
# sapply() simplified 
##########################################################
# Practic
grades <- c(rep('A', 9), rep('A-', 3), rep('B+', 5), rep('B', 7))
group <- factor(grades)
ages <- c(20,21,19,21,19,19,20,19,18,22,21,20,21,22,19,18,19,21,20,19,18,20,21,22)
sex <- c(rep('M',4), rep('F',5), rep('M',3), rep('F',5), 'M', 'M','F', 'M','M','F','F')

table(sex, group)
table(ages, group)

splitdata <- split(ages, list(group))
splitdata_sex <- split(ages, list(group, sex))
splitdata$A.F

mean(splitdata$A.F)
mean(splitdata$`B+.F`)

gradedata <- data.frame(grades, sex, ages)
tapply(ages, gradedata, mean)
sapply(splitdata, mean)

##########################################################################
# examples from reading material

