###############################
## Lecture 12 Student Script ##
###############################

setwd("C://Users//Bangda//Desktop//GR5206 Materials")

## Section I: Databases: SQL and Querying ##

# install.packages("DBI")
# install.packages("RSQLite")
library(DBI)
library(RSQLite)
drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "baseball.db")

# List tables in our database 
dbListTables(con)

# List fields in a table
dbListFields(con, "Batting") 
dbListFields(con, "Pitching")

# Reading tables into R
batting <- dbReadTable(con, "Batting") 
class(batting) 
dim(batting)

# task
salaries <- dbReadTable(con, "Salaries")
str(salaries)
library(plyr)
my.sum.func <- function(team.yr.df){
  return(sum(team.yr.df$salary))
}
total.salaries.team <- ddply(salaries[], .(yearID, teamID), my.sum.func)


# A first query
dbGetQuery(con, paste("SELECT playerID, yearID, AB, H, HR",
                      "FROM Batting",
                      "LIMIT 10"))

################################################################################
# some other query
# 1. query from a table
query1 <- "SELECT playerID, yearID, AB, H, HR FROM Batting LIMIT 20"
dbGetQuery(con, query1)

# 2. query distinct rows from a table
query2 <- "SELECT DISTINCT playerID FROM Batting LIMIT 20"
dbGetQuery(con, query2)

# 3. order the result
query3 <- "SELECT playerID, yearID FROM Batting ORDER BY yearID ASC LIMIT 20"
dbGetQuery(con, query3)

# 4. using an aggregate function
query4 <- "SELECT playerID, MIN(yearID), MAX(yearID) FROM Batting
           GROUP BY playerID LIMIT 5"
dbGetQuery(con, query4)

# 5. filter rows with condition
query5 <- "SELECT * FROM Batting 
          WHERE yearID <= 2000 AND yearID >=1950 LIMIT 5"
dbGetQuery(con, query5)

# 6. query by group
query6 <- "SELECT *[yearID] FROM Batting GROUP BY playerID LIMIT 5"
dbGetQuery(con, query6)

################################################################################

batting[1:10, c("playerID", "yearID", "AB", "H", "HR")]

# ORDER BY
dbGetQuery(con, paste("SELECT playerID, yearID, AB, H, HR",
                      "FROM Batting",
                      "ORDER BY HR DESC",
                      "LIMIT 5"))
# task
batting <- dbReadTable(con, "Batting")
str(batting)
batting1 <- batting[,c("playerID", "yearID", "AB", "H", "HR")]
batting2 <- batting1[batting1$yearID <= 2000 & batting1$yearID >= 1990, ]
batting3 <- batting2[order(batting2$HR, decreasing = TRUE), ]
batting3[1:5, ]

batting[which.max(batting$HR), c("playerID", "yearID", "HR")]

## Section II: Databases: SQL Computations ##

# An SQL computation (use aggregate function)
dbGetQuery(con, paste("SELECT AVG(HR), AVG(H)",
                      "FROM Batting"))
mean(batting$HR, na.rm = TRUE)
mean(batting$H, na.rm = TRUE)

# GROUP BY
dbGetQuery(con, paste("SELECT playerID, AVG(HR)",
                      "FROM Batting",
                      "GROUP BY playerID",
                      "ORDER BY AVG(HR) DESC",
                      "LIMIT 5"))

# task
subset <- batting[batting$yearID >= 1990, ]
mean.hr.team <- daply(subset, .(teamID), function(df){
  return(mean(df$HR, na.rm = TRUE))
})
head(sort(mean.hr.team, decreasing = TRUE), 5)

# WHERE
dbGetQuery(con, paste("SELECT yearID, AVG(HR)",
                      "FROM Batting",
                      "WHERE yearID >= 1990",
                      "GROUP BY yearID",
                      "ORDER BY AVG(HR) DESC",
                      "LIMIT 5"))

# AS
dbGetQuery(con, paste("SELECT yearID, AVG(HR) as avgHR",
                      "FROM Batting",
                      "GROUP BY yearID",
                      "ORDER BY avgHR DESC",
                      "LIMIT 5"))

# HAVING
dbGetQuery(con, paste("SELECT yearID, AVG(HR) as avgHR",
                      "FROM Batting",
                      "WHERE yearID >= 1990",
                      "GROUP BY yearID",
                      "HAVING avgHR >= 4.5",
                      "ORDER BY avgHR DESC"))

# task
query <- 'SELECT teamID, SUM(salary) as payrolls
          FROM Salaries
          WHERE yearID == 2010
          GROUP BY teamID'
df <- dbGetQuery(con, query)


## Section III: Databases: Join ##

# Example
dbGetQuery(con, paste("SELECT *",
                      "FROM Salaries",
                      "ORDER BY playerID",
                      "LIMIT 8"))

query <- paste("SELECT yearID, teamID, 
                       lgID, playerID, HR",
               "FROM Batting", 
               "ORDER BY playerID",
               "LIMIT 7")
dbGetQuery(con, query)

dbGetQuery(con, paste("SELECT yearID, playerID, salary, HR",
                      "FROM Batting JOIN Salaries 
                            USING(yearID, playerID)",
                      "ORDER BY playerID",
                      "LIMIT 7"))

merged <- merge(x = batting, y = salaries, 
                by.x = c("yearID","playerID"), 
                by.y = c("yearID","playerID"))
merged[order(merged$playerID)[1:8], 
       c("yearID", "playerID", "salary", "HR")]

# LEFT JOIN
dbGetQuery(con, paste("SELECT yearID, playerID, salary, HR",
                      "FROM Batting LEFT JOIN Salaries 
                            USING(yearID, playerID)",
                      "ORDER BY playerID",
                      "LIMIT 7"))

# Question
dbGetQuery(con, paste("SELECT playerID, AVG(HR), AVG(salary)",
                      "FROM Batting JOIN Salaries 
                            USING(yearID, playerID)",
                      "GROUP BY playerID",
                      "ORDER BY Avg(HR) DESC",
                      "LIMIT 10"))


# task
query <- 'SELECT yearID, playerID, E
          FROM Fielding
          WHERE yearID >= 1990
          ORDER BY E DESC                     
          LIMIT 10'
dbGetQuery(con, query)

query <- 'SELECT Salaries
          FROM Fielding JOIN Salaries
                USING(yearID, playerID)'

## Section IV: Debugging ##

# Buggy Code
my.plotter = function(x, y, my.list=NULL) {
  if (!is.null(my.list)) 
    plot(my.list, main="A plot from my.list!")
  else
    plot(x, y, main="A plot from x, y!")
}

my.plotter(x=1:8, y=1:8)
my.plotter(my.list=list(x=-10:10, y=(-10:10)^3))

my.plotter()
my.plotter(my.list=list(x=-10:10, Y=(-10:10)^3))


# Using print() in code

my.plotter = function(x, y, my.list=NULL) {
  if (!is.null(my.list)) {
    print("Here is my.list:")
    print(my.list)
    print("Now about to plot my.list")
    plot(my.list, main="A plot from my.list!")
  }
  else {
    print("Here is x:"); print(x)
    print("Here is y:"); print(y)
    print("Now about to plot x, y")
    plot(x, y, main="A plot from x, y!")
  }
}

my.plotter(my.list=list(x=-10:10, Y=(-10:10)^3))
my.plotter(x="hi", y="there")



## Section V: K-means Clustering ##


# An example with K=2
head(iris)
library(ggplot2)
ggplot(data = iris) + 
  geom_point(aes(Petal.Length, Petal.Width, color = Species))

km.out <- kmeans(iris[, 3:4], centers = 2, nstart = 20)
km.out$centers
km.out$cluster

table(km.out$cluster, iris$Species)
iris$cluster <- as.factor(km.out$cluster)
ggplot(data = iris) + 
  geom_point(mapping = aes(Petal.Length, Petal.Width, color = iris$cluster))

# An example with K=3
km.out <- kmeans(iris[, 3:4], centers = 3, nstart = 20)
table(km.out$cluster, iris$Species)
iris$cluster <- as.factor(km.out$cluster)
ggplot(data = iris) + 
  geom_point(mapping = aes(Petal.Length, Petal.Width, color = iris$cluster))

# Initial clustering matters
set.seed(3)
km.out <- kmeans(iris[, 3:4], centers = 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(iris[, 3:4], centers = 3, nstart = 20)
km.out$tot.withinss

# Coding the K-mena algorithm
data <- iris[, 3:4]
clusters <- sample(1:3, nrow(data), replace = TRUE)
centers  <- apply(data, 2, tapply, clusters, mean)
centers

dist <- function(p1, p2) {
  return(sum((p1 - p2)^2))
}

point.assign <- function(point, centers) {
  # Input: one point
  # Output: which cluster center is closest
  return(which.min(c(dist(point, centers[1, ]), 
                     dist(point, centers[2, ]), 
                     dist(point, centers[3, ]))))
}

new.clusters <- function(points, centers) {
  # Input: points and centers
  # Output: new cluster assignment
  return(apply(points, 1, point.assign, centers))
}

new.clus <- new.clusters(points = data, centers = centers)

while(any(new.clus != clusters)) {
  clusters <- new.clus
  centers  <- apply(data, 2, tapply, clusters, mean)
  new.clus <- new.clusters(points = data, centers = centers)
}
table(new.clus, iris$Species)

