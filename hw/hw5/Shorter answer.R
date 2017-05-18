# hwk5: a shorter code
setwd("C://Users//Bangda//Downloads")
library(xlsx)
wtid <- read.xlsx("report.xlsx", sheetName = "Series-layout A", startRow = 2, header = TRUE)
wtid <- wtid[c("Year","Country", "Average.income.per.tax.unit",
               "P99.income.threshold", "P99.5.income.threshold",
               "P99.9.income.threshold")]












