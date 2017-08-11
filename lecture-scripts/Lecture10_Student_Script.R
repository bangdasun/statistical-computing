###############################
## Lecture 10 Student Script ##
###############################

setwd("C://Users//Bangda//Desktop//GR5206 Materials")

## Section I: Selective Access and the apply() Family ##

data(cats, package = "MASS")
head(cats)

# selective access: 1. use logicals!
# dataframe$var1[logical expression]
# One way to find heart weight for male cats
head(cats$Hwt[cats$Sex=="M"])

# Another way: use column names
# [logical expression, colname]
head(cats[cats$Sex=="M","Hwt"])

# selective access: 2. use indices!
# dataframe[indices, ]
# Subsetting with indices
cats.subset <- sample(1:nrow(cats), size = nrow(cats)/2)
head(cats.subset)
new.cats <- cats[cats.subset,]
head(new.cats, 3)

males   <- cats$Sex == "M"
row.ind <- sample(1:nrow(cats), size = nrow(cats)/2)

# The subset() function
# use colnames directly
boy.cats.1 <- subset(cats, Sex == "M")

# Get same thing by extracting the appropriate rows 
# manually
boy.cats.2 <- cats[cats$Sex=="M", ]
all(boy.cats.1 == boy.cats.2)

# Regress heart rate on body weight
coefficients(lm(cats$Hwt ~ cats$Bwt))

# Same calculation
# use with() function, we can refer to columns in a data frame directly by names
coefficients(with(cats, lm(Hwt ~ Bwt)))

# with() function
A <- data.frame(x = 1:10, y = log(1:10))
with(A, x + y)
with(A, plot(x, y, type = "b"))

# remember: non-binary, non-integer vector cannot be used to index data !!! 
# do not do this: dataframe$var1[colname]

# States dataframe
states <- data.frame(state.x77, Region = state.region)

# Matrix of states data, 50 states x 9 variables
head(states, 3)

states$Income["South"] # doesn't work

# This is inefficient and clumsy
# loops are a last resort, not a first

# find the states income & states in "South"
income.south <- c()
for (i in 1:nrow(states)) {
  if (states$Region[i] == "South") {
    income.south <- c(income.south, states$Income[i])
  }
}
income.south

states$Income[states$Region == "South"]

# Same as the above
states[states$Region == "South", "Income"]

# Vectorized Functions
dim(is.na(cats)) # checks each element for being NA

# The apply() function
# lost of functions will automatically apply themselves to each element in a vector or dataframe
# they are vectorized
# if the function doesn't vectorize, or it doesn't quite do what you want, turn to apply() family

# tasks: don't use apply()
length(states$Frost >= 150)
# wrong, should use sum

states[states$Frost >= 150, ]
# should use row.names
rownames(states)[states$Frost >= 150]

ave <- colSums(states[,1:8]) / nrow(states)
colSums(states[,1:8] > rep(ave, each = dim(states)[1]))
##################################################################################
# Maximum entry in each column
apply(states[,1:8], MARGIN = 2, FUN = max)


# Index of the max in each column
apply(states[,1:8], MARGIN = 2, FUN = which.max)

# Summary of each col, get back matrix!
apply(states[,1:5], MARGIN = 2, FUN = summary)


# Rewrite the books so the Northeast gets less frost
frost.fake <- apply(states, 1, function (r) {
  return(ifelse(r[9] == "Northeast", 0.5 * as.numeric(r[7]), as.numeric(r[7])))
})

mean(states$Frost[states$Region == "Northeast"])
mean(frost.fake[states$Region == "Northeast"])

# Rewrite the books so the Northeast gets less frost
frost.reducer <- function (r) {
  frost <- ifelse(r[9] == "Northeast", 0.5 * as.numeric(r[7]), as.numeric(r[7]))
  return(frost)
}
frost.fake2 <- apply(states, 1, frost.reducer)
identical(frost.fake, frost.fake2)


# Function finds indices of biggest 3 entries of v,
# then returns the corresponding elements of names.v

top.3.names = function(v, names.v) { 
  names.v[order(v, decreasing=TRUE)[1:3]] 
}


# Run the function on each column of states. Note: here 
# v is be a column, and names.v is the state names
apply(states[, 1:7], MARGIN = 2, FUN = top.3.names, names.v = rownames(states))

# tasks:
cor.v1.v2 <- function(v1, v2 = states[, "Frost"]){
  return(cor(v1, v2))
}

apply(states[, 1:8], MARGIN = 2, cor.v1.v2)

apply(states[, 1:8], MARGIN = 2, cor, y = states$Frost)

# How to compute the leave-one-out means, also called 
# jackknife means, without a for() loop?
mean.omitting.one <- function(i, vec) { 
  return(mean(vec[-i])) 
}
my.vec <- states[ ,"Frost"]
n      <- length(my.vec)
# my.vec is an additional argument to mean.omitting.one
my.vec.jack <- lapply(1:n, FUN = mean.omitting.one, vec = my.vec)


# It's a list, and here are the first 3 elements
head(my.vec.jack, 3)

# my.vec is an additional argument to mean.omitting.one
my.vec.jack <- sapply(1:n, FUN = mean.omitting.one, vec = my.vec)
# It's a vector, and here are the first 5 elements
head(my.vec.jack)

mean(my.vec)
# Jackknife standard error
sqrt((n-1)^2/n) * sd(my.vec.jack)
# Compare to "usual" standard error of the mean
sd(my.vec)/sqrt(n)

# Let's avg the Frost variable, within in each region
tapply(states[,"Frost"], INDEX=state.region, FUN=mean)

mapply(rep, 1:4, 4:1)
mapply(rep, 1:5, 1:5)

# tasks
states$Division <- state.division
div.means <- tapply(states$Income, INDEX = states$division, mean)
names(div.means)[which.max(div.means)]

# tasks
grad.by.lit.median <- function(df){
  precs <- 100 * df$HS.Grad / (100 - df$Illiteracy) 
  return(median(precs))
}
grad.by.lit.median(states)

## Section II: Transforming Data ##

# Scaling Data
head(scale(cats[,-1], center = TRUE, scale = TRUE), 3)
#   center = TRUE: substract mean from each column
#   scale = TRUE: divides each column by standard deviation

head(cats$Hwt)
head(rank(cats$Hwt))

exp(colMeans(log(states[, 1:8])))

# diff(), cum(fun)

# Let's split up the states matrix according to region

states.by.reg = split(states, f = states$Region)
class(states.by.reg) # The result is a list
names(states.by.reg) # With 4 elements for the 4 regions
class(states.by.reg[[1]]) # Each element is a data frame

# For each region, display first 2 rows of the data frame
lapply(states.by.reg, FUN = head, 2)

# For each region, average the 8 numeric variables
mean.fun <- function(df) {apply(df[, 1:8], MARGIN = 2, mean, na.rm = TRUE)}
lapply(states.by.reg, mean.fun)

# Aggregating Data
aggregate(states[,1:8], by = list(states$Region), mean)



## Section III: Re-ordering and Merging Dataframes ##

# Reordering data
head(cats, 3)
hwt.order <- order(cats$Hwt) # By increasing heart weight
cats.order <- cats[hwt.order, ] # Reorder rows
head(cats.order, 3)

# rank() vs order() vs sort()
this.vec <- c(25, 13, 25, 77, 68)
rank(this.vec)
order(this.vec)
this.vec[order(this.vec)]
sort(this.vec)

# Find the minimum
which.min(cats$Hwt) == order(cats$Hwt)[1]

# Transpose
t(cats)[, 1:5]

# Reshaping
# snoq <- read.csv("snoqulmie.csv", header = TRUE)


# Merging Data
fha <- read.csv("fha.csv", na.strings = "NA", colClasses = c("character", "double", "double", "double"))
nrow(fha)
colnames(fha)
head(fha, 3)

ua <- read.csv("ua.txt", sep = ";")
nrow(ua)
head(ua, 2)

length(unique(fha$Population)) == nrow(fha)
ua.pop.top498 = sort(ua$POP, decreasing = TRUE)[1:nrow(fha)]
max(abs(fha$Population - ua.pop.top498))

# Order by population

ua.sort <- ua[order(ua$POP, decreasing = TRUE), ]
area    <- ua.sort$AREALANDSQMI[1:nrow(fha)]
df1     <- data.frame(fha, area)

# Neaten up names

colnames(df1) <- c("City","Population","Roads","Mileage","Area")

nrow(df1)
head(df1, 3)

df2 <- merge(x = fha, y = ua, by.x = "Population", by.y = "POP")
nrow(df2)
tail(df2, 2)

df2.1 <- merge(x = fha, y = ua, by.x = "City", by.y = "NAME")
nrow(df2.1)
df2.2 <- merge(x = fha, y = ua, by.x = "City", by.y = "NAME", all.x = TRUE)
nrow(df2.2)
df2.2$City[is.na(df2.2$POP)]

# Convert 1,000s of miles to miles
df1$Mileage <- 1000 * df1$Mileage

# Plot daily miles per person vs. area
plot(Mileage/Population ~ Area, data = df1, log = "x",
     xlab = "Miles driven (per person per day)",
     ylab = "City area (sq. miles)")

# Impressively flat regression line
abline(lm(Mileage/Population ~ Area, data = df1), col = "blue")

## Section IV: Re-shaping Dataframes ##

snoq <- read.csv("snoqualmie.csv", header = FALSE, as.is = TRUE)
colnames(snoq) <- 1:366
snoq$year      <- 1948:1983
snoq[1:3, 360:367]

require(reshape2)
snoq.melt <- melt(snoq, id.vars = "year", variable.name = "day", value.name = "precip")
head(snoq.melt)

tail(snoq.melt)
dim(snoq.melt) # 36*366

snoq.melt.chron <- snoq.melt[order(snoq.melt$year, snoq.melt$day), ]
head(snoq.melt.chron)

leap.days <- snoq.melt.chron$day == 366
sum(is.na(snoq.melt.chron$precip[leap.days]))

snoq.melt.chron <- na.omit(snoq.melt.chron)

short.chron <- snoq.melt.chron[-nrow(snoq.melt.chron), ]
precip.next <- snoq.melt.chron$precip[-1]
snoq.pairs  <- data.frame(short.chron, precip.next)
head(snoq.pairs)

snoq.recast <- dcast(snoq.melt, year ~ ...)
dim(snoq.recast)
snoq.recast[1:4, 1:15]
