##############################
## Lecture 4 Student Script ##
##############################

## review
?iris
head(iris)
summary(iris)
dim(iris)
table(iris$Species)


# Q1
subset1 <- iris[iris$Species == 'versicolor' & iris$Petal.Width <= 1.2, ]
dim(subset1)

# A1
sum(iris$Petal.Width[iris$Species == 'versicolor'] <= 1.2)


# Q2
species.group <- factor(iris$Species)
tapply(iris$Petal.Length, species.group, mean)

# Q3
table(iris$Species, iris$Sepal.Width >= 3.0)

# A3
table(iris$Species[iris$Sepal.Width >= 3.0])

# Q4
ifelse(iris$Species == 'setosa', 1, 0)
iris$Setosa <- ifelse(iris$Species == 'setosa', 1, 0)
table(iris$Setosa, iris$Species)


# plot task
hist(iris$Sepal.Width, xlab = 'Sepal Width', breaks = 20)
boxplot(iris$Petal.Length ~ iris$Species)

## Section I: Characters and Character Strings ##


# Mode
mode("d")
mode("cat, squirrel")


# Whitespace: nchar() the number of characters
mode(" ")
nchar(" "); nchar("  "); nchar("")
nchar("sunbangda") # the length of the character

# quotes withinin a strng: \"
# new line: \n
# tab: \t
# space: \s

# Length vs. nchar
length("cat, squirrel, hedgehog")
length(c("cat", "squirrel", "hedgehog")) 
nchar("cat, squirrel, hedgehog") # Not 25, it is 23: 18 letters + 2 comma + 2 space
nchar(c("cat", "squirrel", "hedgehog"))


# Print vs. cat : cat is for characters
print("cat, squirrel")
a <- cat("cat, squirrel")
a
mode(a)
x <- 6
y <- 7
cat("I have", x, "cats and", y, "hedgehogs as pets.")
print("cat, \n squirrel")
cat("cat, \nsquirrel")
print("In R, an \"array\" is a multi-dimension matrix.")
cat("A group of hedgehogs is called an \"array\".")

print("Columbia\tUniversity")
cat("Columbia\tUniveristy")

# Substrings
phrase <- "Christmas Bonus"
substr(phrase, start = 8, stop = 12)
substr(phrase, start = 13, stop = 13) <- "g"
phrase
fav_animals <- c("cat", "squirrel", "hedgehog")
fav_animals
nchar(fav_animals)
substr(fav_animals, start = 1, stop = 2)
substr(fav_animals, nchar(fav_animals)-1, nchar(fav_animals))
substr(fav_animals, start = 4, stop = 4)


# String split
todo <- "Lecture, Lab, Homework"
strsplit(todo, split = ",")
strsplit(todo, split = ", ")
todo <- "Lecture, Lab, Homework"
strsplit(c(todo, "Midterm, Final"), split = ",")

# task
v <- c("Columbia", "slumber party", "sugarplum")
v
substr(v, start = c(3, 2, 7), stop = c(5, 4, 9))
strsplit(v, split = 'lum')

# Paste
paste("cat", "squirrel", "hedgehog")
paste("cat", "squirrel", "hedgehog", sep = ", ")
paste(c("cat", "squirrel", "hedgehog"), 1:3)
paste(c("cat", "squirrel", "hedgehog"), 1:2)
paste(c("cat", "squirrel", "hedgehog"), "(", 1:3, ")")
paste(c("cat", "squirrel", "hedgehog"), "(", 1:3, ")", sep = "")
paste(c("cat", "squirrel", "hedgehog"), " (", 1:3, ")", sep = "")
paste(c("cat", "squirrel", "hedgehog"), " (", 1:3, ")", sep = "", collapse = "; ")

# task
paste(c("Columbia", "slumber party", "sugarplum"), " [", c(3, 2, 7), "-", c(5, 4 ,9), "] ", sep = "", collapse = "; ")

# Honor Code Example
HC <- readLines("C:\\Users\\Bangda\\Desktop\\GR5206 Materials\\HonorCode.txt", warn = FALSE)
length(HC)
head(HC, 5)


# Searching Strings
grep("students", HC)
grep("Students", HC)
head(grepl("students", HC), 15)
grep("students", HC)
HC[grep("students", HC)]


# Honor Code Example
head(HC)
HC <- paste(HC, collapse = " ")                   # One long string
HC.words <- strsplit(HC, split = " ")             # Then split
HC.words <- strsplit(HC, split = " ")[[1]]        # List output
head(HC.words, 10)                                # See the first 10 words
word_count <- table(HC.words)                     # Count the words
word_count <- sort(word_count, decreasing = TRUE) # Sort according to the frequent
head(word_count, 10)                              # 10 words that appears most
tail(word_count, 10)                              # 10 words that appears least

# task
head(word_count)
word_count
grep(";", names(word_count))
names(word_count)[grep(";", names(word_count))]

# plot task
plot(iris$Sepal.Width, iris$Sepal.Length, 'p', col = factor(iris$Species))
legend("topright",
       legend = unique(iris$Species),
       col = 1:length(unique(iris$Species)),
       pch = 1, cex = .5)


library(ggplot2)
ggplot(data = iris) + geom_point(mapping = aes(x = Sepal.Width, y = Sepal.Length))

## Section II: Regular Expressions ##

fav_animals <- "cat,squirrel, hedgehog,   octopus"
fav_animals
strsplit(fav_animals, split = ",")  # split at ,
strsplit(fav_animals, split = " ")  # split at (space)
strsplit(fav_animals, split = ", ") # split at ,(space)


# Matching
grep("cat|dog", c("categorize", "work doggedly", "cag", "cat and dog")) # cat or dog
grep("A|b|a", c("Alabama", "blueberry", "Obama work doggedly")) # A or b
grep("Ab", c("Aerospace", "Obama", "Alabama"))
grep("A\\|b", "Conditional Probability P(A|b)") 

# rules for regular expression
#  1. indicate sets of characters with brackets []
#     "[a-z]" matches any lower case letters
#     "[:punct:]" matches all punctation marks
#  2. the caret ^ negates a character range when in the leading position
#     "[^aeiou]" matches any characters except lower-case vowels
#  3. the period . stands for any character and doesn't need brackets
#     "c..s" matches "cats", "class", "c88s", "c  s", etc
#  4. quantifier: apply to last character before they appear
#     +: repeat one or more times
#     *: repeat zero or more times
#     ?: repeat one or zero times
#     {n}: repeat extacly n times
#     {n, }: repeat n or more times
#     {n, m}: repeat between n and m times
#  5. $ sign means that a pattern only matches at the end of a line
#  6. the caret ^ outside of brackets means that a pattern only matches at the beginning of a line


# examples:
#   "[0-9][0-9][a-zA-Z]+" matches strings with two digits followed by one or more letters
#   "(abc){3}" matches "abcabcabc"
#   "abc{3}" matches "abccc"
#   "M[rs][rs]?\.?" matches "Mr", "Ms", "Mrs", "Mr.", "Ms.", "Mrs."
#   "[a-z,]$" matches strings ending in low-case letters or a comma
#   "^[^A-Z]" matches strings not beginning with capital letters

# strsplit() can use a regular expression to divide a string into a vector
# grep() can search for patterns represented by regular expressions in a string
# regexp()
# regexpr()

# Honor Code Example
HC <- readLines("C:\\Users\\Bangda\\Desktop\\GR5206 Materials\\HonorCode.txt", warn = FALSE)
length(HC)
head(HC, 5)
HC <- paste(HC, collapse = " ") # One long string
HC
HC.words <- strsplit(HC, split=" ")[[1]] # Last Time
HC.words <- strsplit(HC, split="(\\s|[[:punct:]])+")[[1]] # \s means space. split according to space and punctation
# what if university's ?
# split = "\\s+|([[:punct:]]+[[:space:]]+)": either any number of white spaces or at least one punctation mark followed by at least one space


help(regexp)

# web data
# 

# Earthquakes Example
quakes <- readLines("C:\\Users\\Bangda\\Desktop\\GR5206 Materials\\NCEDC_Search_Results.html", warn = FALSE) 
head(quakes)
tail(quakes)
quakes[8:15]
length(quakes)

date_express <- "^[0-9]{4}/[0-9]{2}/[0-9]{2}"
quakes2 <- grep(quakes, pattern = date_express)
head(grep(quakes, pattern = date_express))
head(grep(quakes, pattern = date_express, value = TRUE))
quakes2 <- grep(quakes, pattern = date_express, value = TRUE)     # filter data  
grep(quakes, pattern = date_express, invert = TRUE, value = TRUE) # filtered data line


# More grep commands
grep("a[a-z]", "Alabama")                           # is there a match
regexpr("a[a-z]", "Alabama")                        # info about the 1st match
gregexpr("a[a-z]", "Alabama")                       # info on all matches
regmatches("Alabama", gregexpr("a[a-z]", "Alabama"))# what are the matches

# practice
grep("GR[0-9]{4}", c("STAT GR5205", "STAT GR5206", "IEOR E4404"))
grep("^[^5][0-9]{3}", c("5203", "4404")) # fliter the 5 at 1st position
grep("^[5][0-9]{3}", c("5203", "4404"))  # fliter non-5 at 1st position
grep("STAT\\sGR52[0-9]{2}", c("STAT GR5203", "STAT GR5204", "CSOR, W1103", "STAT GR6010", "STAT GR5264"))
grep("[0-9]+\\.[0-9]+", c("13.1", "20", "30.231"))
grep("[A|B|C][0-9]?", c("A1", "B2", "C2", "D1", "A", "AB", "Ab", "BCD"))
grep("-?[0-9]+", c("010", "900", "<head>", ""))


# task
id_express <- "[0-9]{12}$"
grep(quakes, pattern = id_express)

head(grep(quakes, pattern = id_express))
head(grep(quakes, pattern = id_express, value = TRUE))
grep(quakes, pattern = id_express, invert = TRUE, value = TRUE)

# Earthquakes Example 
quakes[11:15]
coord_exp <- "-?[0-9]+\\.[0-9]{4}" # - (0 or 1 time) + digits (more than one time)
full_exp <- paste(coord_exp, "\\s+", coord_exp, sep = "")
full_exp

head(grepl(quakes, pattern = full_exp), 15)
coord_log <- grepl(quakes, pattern = full_exp)
coord_log
matches   <- gregexpr(pattern = full_exp, text = quakes[coord_log])

head(matches, 1)
coords <- regmatches(quakes[coord_log], matches)
head(coords, 4)
coords_split <- sapply(coords, strsplit, split="\\s+") # split by space \s
head(coords_split, 3)
coords_mat <- matrix(unlist(coords_split), ncol = 2, byrow = TRUE)
colnames(coords_mat) <- c("Latitude", "Longitude")
head(coords_mat)

#install.packages("maps")
library(maps)
map("world")
points(coords_mat[,"Longitude"], coords_mat[,"Latitude"], pch = 19, col = "red", cex = .5)


# web scraping ###########################################################################
install.packages("rvest")
install.packages("XML")
install.packages('stringr')
install.packages('tidyr')
library(XML)
library(xml2)
library(rvest)
library(stringr)
library(tidyr)

lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")
course.material <- read_html("https://sites.google.com/site/introlinearregression/course-material")

gr5205 <- html_nodes(course.material, 'table')
gr <- html_table(gr5205, fill = TRUE)[[1]]
gr

lego_movie %>%
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()

lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

# practice 1 ###############################################################################
# scraping 
url <- 'http://www.espn.com/nfl/superbowl/history/mvps'
super.mvp <- read_html(url)

# extract the HTML table element and convert it to a data frame
sb_table <- html_nodes(super.mvp, 'table')
sb <- html_table(sb_table)[[1]]
head(sb)

# remove the first two rows and set the column names
sb <- sb[-(1:2), ]
names(sb) <- c("number", "player", "MVP")
head(sb)

# practice 2 ###############################################################################
# We use the read_html function to read a web page. This function is provided by the xml2 package, 
# which was loaded automatically when we loaded rvest.
url1 <- 'http://espn.go.com/nfl/superbowl/history/winners'
webpage <- read_html(url1)

# Next, we use the functions html_nodes and html_table (from rvest) to 
# extract the HTML table element and convert it to a data frame.
sb_table <- html_nodes(webpage, 'table')
sb <- html_table(sb_table)[[1]]
head(sb)

# We remove the first two rows, and set the column names.
sb <- sb[-(1:2), ]
names(sb) <- c("number", "date", "site", "result")
head(sb)

# It is traditional to use Roman numerals to refer to Super Bowls, but Arabic numerals are more convenient to work with. 
# We will also convert the date to a standard format.
sb$number <- 1:49
sb$date <- as.Date(sb$date, "%B. %d, %Y")
head(sb)

# The result column should be split into four columns - the winning team's name, the winner's score, the losing team's name, and the loser's score. We start by splitting the results column into two columns at the comma. 
# This operation uses the separate function from the tidyr package.
sb <- separate(sb, result, c('winner', 'loser'), sep=', ', remove=TRUE)
head(sb)

# Finally, we split off the scores from the winner and loser columns. The function str_extract from the stringr package finds a substring matching a pattern. 
# In this case, the pattern is a sequence of 1 or more digits at the end of a line.
pattern <- " \\d+$"
sb$winnerScore <- as.numeric(str_extract(sb$winner, pattern))
sb$loserScore <- as.numeric(str_extract(sb$loser, pattern))
sb$winner <- gsub(pattern, "", sb$winner)
sb$loser <- gsub(pattern, "", sb$loser)
head(sb)

# Install the rvest package

######################################################################################
library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

my_html <- read_html(url)

my_tables <- html_nodes(my_html,"table")[[3]]
my_tables
populous_table <- html_table(my_tables)
populous_table
populous_table <- populous_table[,-4:-6]
populous_table$Population <- as.numeric(gsub(",","",populous_table$Population))/100000

names(populous_table) = c("Rank","Country","Population")

# Let's plot the first 10 rows

library(lattice)
xyplot(Population ~ as.factor(Country), populous_table[1:10,],
       scales = list(x = c(rot=60)),type="h",main="Most Densely Populated Countries")


