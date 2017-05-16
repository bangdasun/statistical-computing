# GR5206 Hwk1

# Data import (i)
data <- read.table("C:\\Users\\Bangda\\Desktop\\GR5206 Materials\\Titanic.txt",
                   header = T, sep = "", as.is = T)
head(data)
titanic <- data.frame(data)

# Check dimension (ii)
dim(titanic)

# Create new variable (iii)
titanic$Survived.Word[titanic$Survived == 1] <- "survived"
titanic$Survived.Word[titanic$Survived == 0] <- "died"

##########################################################################################

# Calculate mean (i)
age <- titanic$Age
fare <- titanic$Fare
survived <- titanic$Survived
submat <- cbind(survived, age, fare)
apply(submat, 2, mean)

# Compute the proportion of female who survived (ii)
group_sex <- factor(titanic$Sex)
survived_list <- split(titanic$Survived, group_sex)
prop <- mean(survived_list$female)
prop <- round(prop, digits = 2); prop

# Compute the proportion of female passengers (iii)
num_male_survived <- sum(survived_list$male)
num_female_survived <- sum(survived_list$female)
prop2 <- num_female_survived / (num_male_survived + num_female_survived)
prop2 <- round(prop2, digits = 2); prop2

# Create new vector and fill in (iv)
classes <- sort(unique(titanic$Pclass))
Pclass.Survival <- vector("numeric", length = 3)
names(Pclass.Survival) <- classes

for (i in 1:3){
  Pclass.Survival[i] <- round(mean(titanic$Survived[titanic$Pclass == i]), digits = 2)
}

# Use tapply() to solve the above problem (v)
group_Pclass <- factor(titanic$Pclass)
survived_rate <- tapply(titanic$Survived, group_Pclass, mean)
survived_rate <- round(survived_rate, digits = 2); survived_rate
