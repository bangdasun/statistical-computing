##############################
## Lecture 6 Student Script ##
##############################


## Section I: Some More Plotting with Base R ##

# Diamonds dataset
setwd("C://Users//Bangda//Desktop//GR5206 Materials//week 6")
diamonds <- read.csv("diamonds.csv", as.is = TRUE)
diamonds <- read.csv("diamonds.csv", as.is = T)

# factor cut
diamonds$cut     <- factor(diamonds$cut)
# factor color
diamonds$color   <- factor(diamonds$color)
# factor clarity
diamonds$clarity <- factor(diamonds$clarity)

set.seed(1)
rows <- dim(diamonds)[1]
diam <- diamonds[sample(1:rows, 1000), ]

# Scatterplot
plot(log(diam$carat), log(diam$price), col = diam$cut)
cut.level <- levels(diam$cut)
legend("bottomright", legend = cut.level, fill = 1:5, cex = 1)

# Adding lines
abline(8, 0, col = "orange", lty = 2)
abline(h = 9, col = "red", lty = 1)         # horizontal line
abline(v = -.5, col = "blue", lty = 2)      # vertical line
lm1 <- lm(log(diam$price) ~ log(diam$carat))
abline(lm1)

# Adding regression lines
cuts        <- levels(diam$cut)
col_counter <- 1

for (i in cuts) {
  this_cut    <- diam$cut == i
  this_data   <- diam[this_cut, ]
  this_lm     <- lm(log(this_data$price) 
                    ~ log(this_data$carat))
  abline(this_lm, col = col_counter)
  col_counter <- col_counter + 1
}

# Adding points and text
points(-0.4, 6.8,  pch = "*", col = "purple")
text(-0.4, 6.8 - .2, "New Diamond", cex = .5)

# task
attach("iris")
iris$Setosa <- as.numeric(iris$Species == "setosa")
iris$Setosa <- factor(ifelse(iris$Species == "setosa", 1, 0))
plot(iris$Sepal.Width, iris$Petal.Length, col = iris$Setosa, xlab = "Width", ylab = "Length") # ???????


# task
Grocery <- read.table("Kutner_6_9.txt", header = TRUE)
newGrocery <- data.frame(LaborHours = Grocery$Y,
                         Cases = Grocery$X1,
                         IndirectCosts = Grocery$X2,
                         Holiday = Grocery$X3)
plot(newGrocery$LaborHours, newGrocery$Cases)
points(4700, 400, pch = "+", col = "red")

## Section III: Advanced Visualization Techniques ##

# install.packages("ggplot2")
library(ggplot2)

dim(mpg)
head(mpg, 3)

# A simple plot
# begin a plot with ggplot()

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

# task
ggplot(data = mpg)

ggplot(data = mpg) + geom_point(mapping = aes(x = cyl, y = hwy))
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))

# Mapping Aesthetics
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# task 
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class)) # or using size = 

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Facets
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(drv~ class)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv~ class)

# Geometric Objects
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Layering geoms
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Adding text and axis labels
p <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(x=3, y=30), color = "purple") +
  geom_text(mapping = aes(x=3, y=31, label = "New Point"), size=4) +
  labs(title = "New Plot", x = "Engine Weight", y = "Highway mpg")

# Some more examples
ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price), alpha = .05)

# task
ggplot(data = iris) +
  geom_point(mapping = aes(x = Sepal.Width, y = Sepal.Length, col = Setosa))

set <- iris$Setosa == 1
set_data <- iris[set, ]
set_lm <- lm(set_data$Sepal.Length~set_data$Sepal.Width)

not.set_data <- iris[!set, ]
not.set_lm <- lm(not.set_data$Sepal.Length~not.set_data$Sepal.Width)
ggplot(data = iris) + 
  geom_point(mapping = aes(x = Sepal.Width, y = Sepal.Length, col = Setosa)) +
  labs(x = "Sepal Width", y = "Sepal Length") +
  geom_abline(intercept = coef(set_lm)[1], slope = coef(set_lm)[2], color = 4) +
  geom_abline(intercept = coef(not.set_lm)[1], slope = coef(not.set_lm)[2], color = 2)


# task
ggplot(data = newGrocery) + geom_point(mapping = aes(x = LaborHours, y = Cases, color = Holiday)) + 
  geom_point(mapping = aes(x = 4700, y = 400), shape = "+", color = "red", size = 5) +
  geom_text(mapping = aes(x = 4700, y = 410, label = "New Point"), size = 4) 

#######################################################################################################
# examples from reading material

# 1. R for data science, chapter 3 data visualization

# 3.1 Introduction
# 3.1.1 prereqiusites
install.packages("tidyverse")
library(tidyverse)

# 3.2 First Setps
# 3.2.1 the mpg data frame
library(ggplot2)
mpg
?mpg

# 3.2.2 creating a ggplot
ggplot(data = mpg) +                                 # create a coordinate system
  geom_point(mapping = aes(x = displ, y = hwy))      # add layer
# each geom function takes a mapping argument, this define how variables in your dataset are mapped
# to visual properties. The mapping argument is always paired with aes(), 
# ggplot(data = <DATA>) + 
#    <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# 3.3 Aesthetic Mappings
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, col = class))

# it is not a good idea to mapping an unordered variable(categorical class) to an ordered aesthetic(size)
# better choice is shape and alpha

# shape: for all data, shape goes out the aes(). 25 classes
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), col = "blue", shape = 0)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), col = "blue", shape = 1)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), col = "blue", shape = 2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), col = "blue", shape = 5)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), col = "blue", shape = 3)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), col = "blue", shape = 4)

# 3.5 Facets

# 3.6 Geometric Objects
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, col = drv))

# combine two same variable using geom function
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(col = class)) + 
  geom_smooth()

# 3.7 Statistical Transformation
# bar chart
# on the x-axis, the chart displays cut, on the y-axis, it display count, which is not
# a variable in the original dataset. 
# the alogrithm used calculated new values for a graphic is called stat (statistical transformation)

# you can learn which stat a geom uses by inspecting the default value for the stat argument
# for example: ?geom_bar shows the default value for stat is 'count' ('prop' is another)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

# this works because every geom has a default stat; and every stat has a default geom
# this means that you can typically use geoms without worrying about the underlying statistical transformation
# there are 3 reasons you might need to use a stat explicitly:
#   1. you might want to override the default stat. In the code below, 
# I change the stat of geom_bar() from count (the default) to identity. 
# This lets me map the height of the bars to the raw values of a  yy variable. 
# Unfortunately when people talk about bar charts casually, 
# they might be referring to this type of bar chart, 
# where the height of the bar is already present in the data, 
# or the previous bar chart where the height of the bar is generated by counting rows.
install.packages("tibble")
library(tibble)
demo <- tribble(
  ~a,   ~b,
  "bar_1", 20,
  "bar_2", 30,
  "bar_3", 40
)
ggplot(data = demo) + 
  geom_bar(mapping = aes(x = a, y = b), stat = "identity")

#  2. you might want to override the default mapping from transformed variables to 
# aesthetics. For example, you might want to display a bar chart of proportion rather than count
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

#  3. you might want to draw greater attention to the statistical transformation in your code.
# For example, you might use stat_summary(), which summarises the y values for each unique x value,
# to draw attention to the summary that you're computing
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

# 3.8 Position Adjustments
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, col = cut))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = cut))

# if map the fill to another variable, the bars are auto stacked
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# the stacking is performed by position adjustment specified by the position
# three arguments: identity, dodge, fill
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) +
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(position = "fill")

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(position = "dodge")

# 3.9 Coordinate Systems
# coord_flip() switches the x and y axes. This is useful, if you want to horizontal boxplot
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

# coord_quickmap() sets the aspect ratio correctly for maps. This is very important if you
# are plotting spatial data with ggplot2
nz <- map_data("state")
ggplot(nz, aes(long, lat, group = group)) + 
  geom_polygon(fill = "white", col = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# coord_polar() uses polar coordinates
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()
