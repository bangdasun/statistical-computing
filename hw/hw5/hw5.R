# i
percentile_ratio_discrepancies <- function(P99, P99.5, P99.9, a){
  # input P99, P99.5, P99.9
  # return the sum square
  sse <- ((P99 / P99.9)^(-a+1) - 10)^2 + 
    ((P99.5 / P99.9)^(-a+1) - 5)^2 + 
    ((P99 / P99.5)^(-a+1) - 2)^2
  return(sse)
}
# Check the return
percentile_ratio_discrepancies(P99 = 1e6, P99.5 = 2e6, P99.9 = 1e7, a = 2)

# ii
exponent.multi_ratios_est <- function(P99, P99.5, P99.9){
  # input P99, P99.5, P99.9
  # minimize the percentile_ratio_discrepancies using nlm()
  # initial value a determined by formula (4)
  a   <- (1 - (log(10) / log(P99 / P99.9)))
  est <- nlm(percentile_ratio_discrepancies, a, P99 = P99, P99.5 = P99.5, P99.9 = P99.9)
  return(est$estimate)
}
exponent.multi_ratios_est(P99 = 1e6, P99.5 = 2e6, P99.9 = 1e7)

# iii
# set work directory and import data
setwd("C://Users//Bangda//Desktop//GR5206 Materials//Hwk5")
wtid <- read.csv("wtid-report.csv", header = TRUE)
str(wtid)
names(wtid) <- c("Country", "Year", "P99", "P99.5", "P99.9")
# function a.estimate 
a.estimate <- function(P99, P99.5, P99.9){
  # input P99, P99.5, P99.9
  # using exponent.multi_ratios_est to estimate each years' a
  a1 <- rep(NA, length(P99))
  for (i in 1:length(a1)){
    # if one of P99, P99.5, P99.9 is missing, return NA
    if (is.na(P99[i]) | is.na(P99.5[i]) | is.na(P99.9[i])){
      a1[i] <- NA
    } else {
      # if P99, P99.5, P99.9 all exists, estimate a
      a1[i] <- exponent.multi_ratios_est(P99 = P99[i], P99.5 = P99.5[i], P99.9 = P99.9[i])
    }
  }
  return(a1)
}
a1 <- a.estimate(P99 = wtid$P99, P99.5 = wtid$P99.5, P99.9 = wtid$P99.9)
df1 <- data.frame(Year = wtid$Year, a1 = a1)
library(ggplot2)
ggplot(df1, mapping = aes(x = Year, y = a1)) + geom_line() + 
  labs(x = "Year", y = "Estimation of a")

# iv
# using equation (4) to estimate
a2 <- (1 - (log(10)) / (log(wtid$P99 / wtid$P99.9))) 
df2 <- data.frame(a1 = a1, a2 = a2)
ggplot(df2, mapping = aes(x = a1, y = a2)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  labs(x = "Estimation of a using a.estimate", y = "Estimation of a using equation (4)")
identical(a1, a2)

# v
library(xlsx)
# import the .xlsx file and save them into data.list
# data.list should has 9 data frame to store 9 countries' data
data.files <- list.files(pattern = "*.xlsx")
data.list <- vector("list", length(data.files))
for (i in 1:length(data.files)){
  data.list[[i]] <- read.xlsx(data.files[i], sheetName = "Series-layout A", 
                              startRow = 2, header = TRUE) 
}

# convert the all countries' data in a single dataframe
wtid3 <- data.frame(Country = character(0), Year = numeric(0),
                    AveIncomePerTU = numeric(0), P99 = numeric(0),
                    P99.5 = numeric(0), P99.9 = numeric(0))
for (j in 1:length(data.files)){
  # check if the variables exist in the dataframe
  if("P99.income.threshold" %in% colnames(data.list[[j]]) & 
     "P99.5.income.threshold" %in% colnames(data.list[[j]]) & 
     "P99.9.income.threshold" %in% colnames(data.list[[j]]) &
     "Average.income.per.tax.unit" %in% colnames(data.list[[j]])){
    # insert the value into the new dataframe
    wtid3 <- rbind(wtid3, data.frame(
      Country = data.list[[j]]$Country,
      Year = data.list[[j]]$Year,
      AveIncomePerTU = data.list[[j]]$Average.income.per.tax.unit,
      P99 = data.list[[j]]$P99.income.threshold,
      P99.5 = data.list[[j]]$P99.5.income.threshold,
      P99.9 = data.list[[j]]$P99.9.income.threshold
    ))
  } else {
    # if the variables are not in the dataframe, create them
    data.list[[j]]$Average.income.per.tax.unit <- rep(NA, length(data.list[[j]]$Year))
    data.list[[j]]$P99.income.threshold <- rep(NA, length(data.list[[j]]$Year))
    data.list[[j]]$P99.5.income.threshold <- rep(NA, length(data.list[[j]]$Year))
    data.list[[j]]$P99.9.income.threshold <- rep(NA, length(data.list[[j]]$Year))
    wtid3 <- rbind(wtid3, data.frame(
      # insert the value NA into the new dataframe
      Country = data.list[[j]]$Country,
      Year = data.list[[j]]$Year,
      AveIncomePerTU = data.list[[j]]$Average.income.per.tax.unit,
      P99 = data.list[[j]]$P99.income.threshold,
      P99.5 = data.list[[j]]$P99.5.income.threshold,
      P99.9 = data.list[[j]]$P99.9.income.threshold
    ))
  }
}
# estimate a for each country and each year
wtid3$a <- a.estimate(P99 = wtid3$P99, P99.5 = wtid3$P99.5, P99.9 = wtid3$P99.9)

# vi
ggplot(data = wtid3) + 
  geom_line(mapping = aes(x = Year, y = a, col = Country))

# vii
library(gridExtra)
g1 <- ggplot(data = wtid3) + 
  geom_line(mapping = aes(x = Year, y = AveIncomePerTU, col = Country)) +
  labs(x = "Year", y = "Average income \nper tax unit")
g2 <- ggplot(data = wtid3) + 
  geom_line(mapping = aes(x = Year, y = AveIncomePerTU, col = Country)) +
  scale_y_log10() + 
  labs(x = "Year", y = "logarithm of Average income \nper tax unit")
grid.arrange(g1, g2, ncol=1)

# viii
# subset US data
wtid.us <- wtid3[wtid3$Country == "United States", ]
# ploting
ggplot(data = wtid.us) +
  geom_point(mapping = aes(x = AveIncomePerTU, y = a))

# ix
lm1 <- lm(a ~ AveIncomePerTU + I(AveIncomePerTU^2), data = wtid.us)
summary(lm1)

# x
reg <- function(country){
  # check if any necessary data exists
  if (all(is.na(wtid3$a[wtid3$Country == country]))){
    return(cat("Data of", country,"is missing."))
  } else {
    lm <- lm(a ~ AveIncomePerTU + I(AveIncomePerTU^2), data = wtid3[wtid3$Country == country,])
    return(summary(lm))
  }
}
reg("Canada")
reg("China")
reg("Colombia")
reg("Germany")
reg("Italy")
reg("Japan")
reg("South Africa")
reg("Sweden")
