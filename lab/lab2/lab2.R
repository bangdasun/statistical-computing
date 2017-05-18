### Part a ###
# i
diamonds <- read.csv("C:\\Users\\Bangda\\Desktop\\GR5206 Materials\\GR5206 Lab2\\diamonds_small.csv", header = T)
lm0 <- lm(price ~ carat, data = diamonds)
coefficients(lm0) # The coefficients of lm0

### Part b ###
# i
n <- dim(diamonds)[1]; n 

# ii
resample1 <- sample(1:n, n, replace = TRUE)

B <- 1000
resampled_values <- matrix(NA, nrow = B, ncol = n)
for (b in 1:B) {
  resampled_values[b, ] <- sample(1:n, n, replace = TRUE)
}
dim(resampled_values)

# iii
resampled_data <- diamonds[resample1,]
resample1_ests <- coefficients(lm(price ~ carat, data = resampled_data))
resample1_ests

# iv
ptm <- proc.time()
lm_func <- function(row){
  lm_rlt <- lm(price ~ carat, data = diamonds[row, ])
	return(coefficients(lm_rlt)) 
}
result <- apply(resampled_values, 1, lm_func)
proc.time() - ptm

ptm <- proc.time()
resampled_ests <- matrix(NA, nrow = B, ncol = 2)
names(resampled_ests) <- c("Intercept_Est", "Slope_Est")
for (b in 1:B) {
  resampled_ests[b,] <- coefficients(lm(price ~ carat, data = diamonds[resampled_values[b, ], ]))
}
proc.time() - ptm
head(resampled_ests)

# v
diff_estimates <- resampled_ests[, 2] - coefficients(lm0)[2]
length(diff_estimates)

# vi
hist(resampled_ests[ ,2], xlab = "beta1_hat", main = "Histogram of bootstrap estimates of beta1_hat")

# vii
sd_beta0 <- sd(resampled_ests[, 1]); sd_beta0
sd_beta1 <- sd(resampled_ests[, 2]); sd_beta1

# viii
Cl <- quantile(diff_estimates, 0.025) + coefficients(lm0)[2]
Cu <- quantile(diff_estimates, 0.975) + coefficients(lm0)[2]
int <- c(Cl, Cu)
int
