### Part 1 ###
# i
housing <- read.csv("C:\\Users\\Bangda\\Desktop\\GR5206 Materials\\NYChousing.csv", header = T)

# ii
rows <-nrow(housing); rows
columns <- ncol(housing); columns

# iii
apply(is.na(housing), 2, sum)

# iv
housing2 <- housing[!is.na(housing$Value),] # housing2 is the dataframe without the NA value in Value

# v
na.rows <- nrow(housing) - nrow(housing2); na.rows # number of rows being removed

# vi
housing2$logValue <- log(housing2$Value) 
summary.logValue <- rep(0,4)
names(summary.logValue) <- c("Min", "Median", "Mean", "Max")
summary.logValue[1] <- min(housing2$logValue)    # min
summary.logValue[2] <- median(housing2$logValue) # median
summary.logValue[3] <- mean(housing2$logValue)   # mean
summary.logValue[4] <- max(housing2$logValue)    # max
summary.logValue

# vii
housing2$logUnits <- log(housing2$UnitCount)

# viii
housing2$after1950 <- (housing2$YearBuilt >= 1950)

### Part 2 EDA ###
# i
plot(housing2$logUnits, housing2$logValue,'p', xlab = 'logUnits', ylab = 'logValue')

# ii
housing2$after1950 <- factor(housing2$after1950)
plot(housing2$logUnits, housing2$logValue, xlab = 'logUnits', ylab = 'logValue', col = housing2$after1950)
legend("bottomright", legend = levels(housing2$after1950), fill
= unique(housing2$after1950))

# iii
cor(housing2$logValue, housing2$logUnits) # whole data
cor(housing2$logValue[which(housing2$Borough == 'Manhattan')], housing2$logUnits[which(housing2$Borough == 'Manhattan')]) # Manhattan
cor(housing2$logValue[which(housing2$Borough == 'Brooklyn')], housing2$logUnits[which(housing2$Borough == 'Brooklyn')]) # Brooklyn
cor(housing2$logValue[which(housing2$after1950 == T)], housing2$logUnits[which(housing2$after1950 == T)]) # after 1950
cor(housing2$logValue[which(housing2$after1950 == F)], housing2$logUnits[which(housing2$after1950 == F)]) # before 1950

# iv
par(mfrow = c(1,2))
plot(housing2$logUnits[which(housing2$Borough == 'Manhattan')], housing2$logValue[which(housing2$Borough == 'Manhattan')], 
     ylab = 'logValue', xlab = 'logUnits', main = "Manhattan", ylim = c(7,21), xlim = c(0,9))
plot(housing2$logUnits[which(housing2$Borough == 'Brooklyn')], housing2$logValue[which(housing2$Borough == 'Brooklyn')],
     ylab = 'logValue', xlab = 'logUnits', main = "Brooklyn",ylim = c(7,21), xlim = c(0,9))

# v
manhat.props <- c()
for (props in 1:nrow(housing2)) {
  if (housing2$Borough[props] == "Manhattan") {
      manhat.props <- c(manhat.props, props)
  }
}
med.value <- c()
for (props in manhat.props) {
  med.value <- c(med.value, housing2$Value[props])
}
med.value <- median(med.value, na.rm = TRUE)

med.value2 <- median(housing2$Value[which(housing2$Borough == 'Manhattan')]); med.value2
med.value == med.value2 # check if they are equal

# vi
boxplot(housing2$logValue ~ housing2$Borough)

# vii
median <- boxplot(housing2$logValue ~ housing2$Borough)$stat[3,]
names(median) <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
median <- exp(median); median # median value for five boroughs
