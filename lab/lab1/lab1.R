# i
normal100 <- rnorm(n = 100, mean = 0, sd = 1); normal100
mean_normal100 <- mean(normal100); mean_normal100
sd_normal100 <- sd(normal100); sd_normal100

# ii
hist(normal100, col = "lightblue", border = "white")

# iii
normal10 <- rnorm(n = 10, mean = 0, sd = 1)
normal1000 <- rnorm(n = 1000, mean = 0, sd = 1)
normal10000 <- rnorm(n = 10000, mean = 0, sd = 1)
normal100000 <- rnorm(n = 100000, mean = 0, sd = 1)

# iv
sample_means <- rep(0, 5)
sample_means[1] <- mean(normal10)
sample_means[2] <- mean(normal100)
sample_means[3] <- mean(normal1000)
sample_means[4] <- mean(normal10000)
sample_means[5] <- mean(normal100000)
print(sample_means)
length(sample_means)

# v
normal1mil <- rnorm(1000000, mean = 3, sd = 2)
mean_normal1mil <- mean(normal1mil); mean_normal1mil
sd_normal1mil <- sd(normal1mil); sd_normal1mil

# vi
x <- which(normal1mil > 3) # identify the elements greater than 3
normal1mil_greatthan3 <- normal1mil[x] # elements greater than3
mean(normal1mil_greatthan3)

# vii
normal1mil_mat <- matrix(normal1mil, ncol = 10000)

# viii
mean_col1234 <- mean(normal1mil_mat[, 1234]); mean_col1234

# ix
normal1mil_mat_colsum <- colSums(normal1mil_mat)
normal1mil_mat_colmean <- normal1mil_mat_colsum / dim(normal1mil_mat)[1]

# x
h <- hist(normal1mil_mat_colmean, breaks = 50, col = "lightblue", border = "white")
qqnorm(normal1mil_mat_colmean)
