##############################
## Lecture 3 Student Script ##
##############################


## Section I: Regression ##


Grocery <- read.table("Kutner_6_9.txt", header=T)
head(Grocery)

X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3)
beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% Grocery$Y
round(t(beta_hat), 2)


# Using R built-in functions for regression

lm0 <- lm(Y ~ X1 + X2 + X3, data = Grocery)
lm0
residuals(lm0)[1:5] 
fitted(lm0)[1:5]

summary(lm0)



## Section IV: Bootstrap ##

# Include so that you get the same random samples as I do in the notes:
set.seed(1)
mu = 0


# Generating a random sample

n <- 100
vec <- rnorm(n, mean = mu)
head(vec)

hat_mu <- mean(vec)

# Getting bootstrap samples

B <- 1000
estimates <- vector(length = B)
for (b in 1:B) {
  new_sample <- sample(vec, size = n, replace = TRUE)
  estimates[b] <- mean(new_sample)
}
head(estimates)


# Histogram of the original sample and a boot strap sample (won't exactly match the notes)

par(mfrow = c(1,2))
hist(vec, xlab = "Values", ylab = "Frequency", main = "Original Sample")
hist(new_sample, xlab = "Values", ylab = "Frequency", main = "A Bootstrap Sample" )


# Histogram of the bootstrap distribution

par(mfrow = c(1,1))
centered_ests <- estimates - hat_mu
hist(centered_ests, xlab = "Bootstrap Sample Means", ylab = "Frequency", 
     main = "Centered Bootstrap Estimates of the Mean")

# Variance of the Estimates

var(estimates)


## Section V: EDA ##


diamonds         <- read.csv("C:\\Users\\Bangda\\Desktop\\GR5206 Materials\\diamonds.csv", as.is = T)

# Factorize

diamonds$cut     <- factor(diamonds$cut)
diamonds$color   <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)


# Bargraphs

table(diamonds$cut) 
names(table(diamonds$cut))
barplot(height = table(diamonds$cut), names.arg = names(table(diamonds$cut)))

# Re-leveling Cut

levels(diamonds$cut)
diamonds$cut <- factor(diamonds$cut, level = c("Fair","Good", "Very Good", "Premium", "Ideal"))
levels(diamonds$cut)

barplot(height = table(diamonds$cut), names.arg = names(table(diamonds$cut)))


# Histograms

hist(diamonds$carat, main = "Histogram of Carats", xlab = "Carats")
hist(diamonds$carat[diamonds$carat < 3], breaks = 100, main = "Histogram of Carats", xlab = "Carats")



# Boxplot

boxplot(price ~ cut, data = diamonds, ylab = "Price", xlab = "Cut")

# Scatterplot


plot(diamonds$carat, diamonds$price, xlab = "Carats", ylab = "Price ($)")

boxplot(carat ~ cut, data = diamonds, ylab = "Carats", xlab = "Cut")

## Section VI: More Plotting ##

# Run to get the same plots as in the notes

set.seed(1)

# Getting a subset of the data

rows       <- dim(diamonds)[1]
small_diam <- diamonds[sample(1:rows, 1000), ]

# Plotting lines

plot(log(small_diam$carat), log(small_diam$price), col = small_diam$cut, xlab = "Log(Carat)", ylab = "log(Price)")
legend("bottomright", legend = levels(small_diam$cut), fill = 1:length(levels(small_diam$cut)), cex = .5)


abline(8, 0, col = "orange", lty = 2)
lm1 <- lm(log(small_diam$price) ~ log(small_diam$carat))
abline(lm1)


# Plotting all the regression lines:

plot(log(small_diam$carat), log(small_diam$price), col = small_diam$cut, xlab = "Log(Carat)", ylab = "log(Price)")
legend("bottomright", legend = levels(small_diam$cut), fill = 1:length(levels(small_diam$cut)), cex = .5)


cuts        <- levels(small_diam$cut)
col_counter <- 1

for (i in cuts) {
  this_cut    <- small_diam$cut == i
  this_data   <- small_diam[this_cut, ]
  this_lm     <- lm(log(this_data$price) 
                    ~ log(this_data$carat))
  abline(this_lm, col = col_counter)
  col_counter <- col_counter + 1
}

# Adding points and texts

points(-0.4, 6.8,  pch = "*", col = "purple")

text(-0.4, 6.8 - .2, "New Diamond", cex = .5)


##########################################################################
# examples from reading material

# 1. R for data science, chapter 7 exploratory data analysis

# generate questions about data -> search for answers by visualization & 
# transforming & modeling data -> refine the questions -> new questions

library(ggplot2)
library(dplyr)
data("diamonds")

# questions:
#   1. what type of variation occurs within variables ?
#   2. what type of covariation occurs between variables ?

# 7.3.1 visuzlization distributions

# categorical data are usually saved as factors or character vectors (use bar plot)
ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))

# count the height of bar plot 
diamonds %>%
  count(cut)

# you should always explore a variety of binwidths when working with histogram
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.1)
diamonds %>%
  count(cut_width(carat, 0.5))

ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.05)
diamonds %>%
  count(cut_width(carat, 0.1))

ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.08)
diamonds %>%
  count(cut_width(carat, 0.2))

ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.01)
diamonds %>%
  count(cut_width(carat, 0.3))

par(mfrow = c(1,1)) # seems no use in ggplot...
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.1)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.05)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.08)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = carat), binwidth = 0.01)

# filter data
small_data <- diamonds %>%
  filter(diamonds$carat < 3)
ggplot(data = small_data, mapping = aes(x = carat)) + geom_histogram(binwidth = 0.1)

# overlap on histogram
ggplot(data = small_data, mapping = aes(x = carat, color = cut)) + geom_freqpoly(binwidth = 0.1)


# 7.3.2 typical values
# questions: 
#   1. which values are the most common, why ?
#   2. which values are rare ? Does that match your expectation ?
#   3. Can you see any unusual patterns ? What might explain them ?

# an example:
#   1. why are there more diamonds at whole carats and common fractions of carats ?
#   2. why are there more diamonds slightly to the right of each peak than there are slightly to the left of each peak ?
#   3. why are there no diamonds bigger than 3 carats ?

ggplot(data = diamonds, mapping = aes(x = carat)) + geom_histogram(binwidth = 0.02)
ggplot(data = diamonds, mapping = aes(x = carat)) + geom_histogram(binwidth = 0.01)

# clusters of similar values suggests that subgroups exist in your data. Question:
#   1. how are the observatios within each cluster similar to each other ?
#   2. how are the observations in separate clusters different from each other ?
#   3. how can you explain or describe the clusters ?
#   4. why might the appearance of clusters be misleading ?

ggplot(data = faithful, mapping = aes(x = eruptions)) + geom_histogram(binwidth = 0.25)


# 7.3.3 unusual values
# outliers are observations that are unusual;
# data points that don't seem to fit the pattern.
# outliers can be error, can be some important new science.
# when you have a lot of data, outliers are sometimes difficult to see in a histogram

ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# zoom into small values of the y-axis with coord_cartesian()
ggplot(diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5) + coord_cartesian(ylim = c(0, 50))
# there are three unusual values
unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  arrange(y)
unusual

# 7.3.4 excises
#   1. 
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = x), bins = 500)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), bins = 500)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = z), bins = 500)

#   2. 
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 10)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 50)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 100)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 500)

#   3.
specific.carat <- diamonds %>%
  filter(carat == 0.99) %>%
  arrange(carat)
specific.carat

specific.carat2 <- diamonds %>%
  filter(carat == 1) %>%
  arrange(carat)
specific.carat2

# 7.4 missing values

# 1. drop the entire row with the strange values
diamonds2 <- diamonds %>%
  filter(between(y, 3, 20)) 
# but it is not recommended, just ont measurement is invalid is not enough
# doesn't mean all the measurements are.


# 2. (alternative methods) replace the unusual values with missing values
diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x , y = y)) + geom_point(na.rm = T)

# compare the missing values and non missing values
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) + geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)


# 7.5 covariation
# 7.5.1 a categorical and continuous variable
#   it is common to want to explore the distribution of a continuous variable broken
#   down by a categorical, as in the previous frequency polygon. The default appearance of
#   geom_freqpoly() is not very useful. That means if one of the groups is much smaller than
#   the others, it is hard to see the differences in shape.

ggplot(data = diamonds, mapping = aes(x = price)) + geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

# it is hard to see the difference in distribution because the overall counts differ so much
ggplot(diamonds) + geom_bar(mapping = aes(x = cut))

# display density
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) + geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

#  there are something rather surprising about this plot: it appears that fair diamonds
#  have the highest average price! this plot is a little hard to interpret

# the distribution of price by cut using
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + geom_boxplot()

# example of mpg data
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + geom_boxplot()

# we can use reorder() to change the order of the factor
ggplot(data = mpg) + geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

# flip it
ggplot(data = mpg) + geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) + coord_flip()

# 7.5.1.1 exercise

# 7.5.2 two categorical variables
# count the number of observation
ggplot(data = diamonds) + geom_count(mapping = aes(x = cut, y = color))
diamonds %>% # need dplyr
  count(color, cut)

# fill aesthetic
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) + geom_tile(mapping = aes(fill = n))

# 7.5.2.1 exercise

# 7.5.3 two continuous variables
ggplot(data = diamonds) + geom_point(mapping = aes(x = carat, y = price))

# scatter plot become useless as the size of datasize increase

ggplot(data = diamonds) + geom_point(mapping = aes(x = price, y = cut))

# fix it: use binning
#   previously, use geom_histogram() and geom_freqpoly() to bin in one dim,
#   now use geom_bin2d() and geom_hex() to bin in two dim
ggplot(data = diamonds) + geom_bin2d(mapping = aes(x = carat, y = price))

install.packages("hexbin")
ggplot(data = diamonds) + geom_hex(mapping = aes(x = carat, y = price))

# another option
#   bin one continuous variable so it acts like a categorical vairable
#   bin carat and then for each group, disploy boxplot
ggplot(data = diamonds, mapping = aes(x= carat, y = price)) + geom_boxplot(mapping = aes(group = cut_width(carat, 0.2)))

# another option
#   display approximately the same number of points in each bin
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

# 7.5.3.1 exercise

# 7.6 patterns and models
# ask yourself:
#   1. could this pattern be due to coincidence ? random chance?
#   2. how can you describe the relationship implied by the pattern?
#   3. how strong is the relationship implied by the pattern?
#   4. what other variables might affect the relationship?
#   5. does the relationship change if you look at individual subgroups of the data?

# longer wait times associated with longer eruptions
ggplot(data = faithful) + geom_point(mapping = aes(x = eruptions, y = waiting))

# variation create uncertainty, covariation reduce it
