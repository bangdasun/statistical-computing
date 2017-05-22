### Part 1 ###
# i
setwd("C://Users//Bangda//Desktop//GR5206 Materials//GR5206 Lab4")
wtiddata <- read.csv("wtid-report.csv", header = TRUE)
head(wtiddata)
# create a new variable
wtiddata2 <- wtiddata[,-1]
names(wtiddata2) <- c("year", "P99", "P99.5", "P99.9")
# P99 in 1972
wtiddata2[wtiddata2$year == 1972, "P99"]
# P99.5 in 1942
wtiddata2[wtiddata2$year == 1942, "P99.5"]
# P99.9 in 1922
wtiddata2[wtiddata2$year == 1922, "P99.9"]

# ii
library(ggplot2)
ggplot(data = wtiddata2[wtiddata2$year <= 2012, ], 
       aes(x = year, y = value, color = variable)) + 
  geom_line(mapping = aes(y = P99, col = "P99"), show.legend = FALSE) + 
  geom_line(mapping = aes(y = P99.5, col = "P99.5"), show.legend = FALSE) + 
  geom_line(mapping = aes(y = P99.9, col = "P99.9"), show.legend = FALSE) + 
  labs(x = "Years", y = "Income")
  
# iii
exponent.est_ratio <- function(P99, P99.9){
  a <- 1 - (log(10)) / (log(P99 / P99.9))
  return(a)
}
# Check the return
exponent.est_ratio(P99 = 1e6, P99.9 = 1e7)

### Part 2 ###
# iv
a <- exponent.est_ratio(P99 = wtiddata2$P99, P99.9 = wtiddata2$P99.9)
ggplot(data = data.frame(year = wtiddata2$year, a = a)) + 
  geom_point(mapping = aes(x = year, y = a))

# v
ratio.95 <- function(P99.5, P99.9, a){
   ratio <- (P99.5 / P99.9)^(-a + 1)
   return(ratio)
}
leftside <- ratio.95(P99.5 = wtiddata2$P99.5, P99.9 = wtiddata2$P99.9, a = a)
ggplot(data.frame(year = wtiddata2$year, rate = leftside)) + 
  geom_line(mapping = aes(x = year, y = rate)) + 
  geom_hline(yintercept = 5)
MSE1 <- mean((leftside - 5)^2); MSE1
  
# vi
ratio.05 <- function(P99, P99.5, a){
   ratio <- (P99 / P99.5)^(-a + 1)
   return(ratio)
}
leftside2 <- ratio.05(P99 = wtiddata2$P99, P99.5 = wtiddata2$P99.5, a = a)
ggplot(data.frame(year = wtiddata2$year, rate = leftside2)) + 
  geom_line(mapping = aes(x = year, y = rate)) + 
  geom_hline(yintercept = 2)
MSE2 <- mean((leftside2 - 2)^2); MSE2
