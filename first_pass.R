# Michael DeWitt


# libraries ---------------------------------------------------------------

library(tidyverse)
library(rjags)


# data import -------------------------------------------------------------

con <- url("https://www4.stat.ncsu.edu/~reich/ABA/assignments/E2.RData")
load(con)

sum(is.na(Y1[,1]) & is.na(Y2))

big_mat <- cbind(Y1, Y2, Y3)

# Model Swag --------------------------------------------------------------

days <- nrow(Y1)
pixels <- ncol(Y1)


# Base model --------------------------------------------------------------

model_test <- "model{

# Likelihood
for(i in 1:days){for(j in 1:pixels){
Y[i,j]    ~ dnorm(meanY[i,j],taue)
meanY[i,j] <- beta[i,j]*response[i,j]
}}

# Random effects
for(i in 1:days){
beta[i,1:6] ~ dmnorm(mu[1:6],Omega[1:6,1:6])
}

# Priors
taue  ~ dgamma(0.1,0.1)
mu[1] ~ dnorm( 0,0.001)
mu[2] ~ dnorm( 0,0.001)
mu[3] ~ dnorm( 0,0.001)
mu[4] ~ dnorm( 0,0.001)
mu[5] ~ dnorm( 0,0.001)
mu[6] ~ dnorm( 0,0.001)

Omega[1:6, 1:6] ~ dwish(R[,], 1)

R[1,1] <- 1/2.1
R[2,2] <- 1/2.1
R[3,3] <- 1/2.1
R[4,4] <- 1/2.1
R[5,5] <- 1/2.1
R[6,6] <- 1/2.1

#Other sides
R[1,2] <- 0
R[1,3] <- 0
R[1,4] <- 0
R[1,5] <- 0
R[1,6] <- 0

R[2,1] <- 0
R[2,3] <- 0
R[2,4] <- 0
R[2,5] <- 0
R[2,6] <- 0

R[3,1] <- 0
R[3,2] <- 0
R[3,4] <- 0
R[3,5] <- 0
R[3,6] <- 0

R[4,1] <- 0
R[4,2] <- 0
R[4,3] <- 0
R[4,5] <- 0
R[4,6] <- 0

R[5,1] <- 0
R[5,2] <- 0
R[5,3] <- 0
R[5,4] <- 0
R[5,6] <- 0

R[6,1] <- 0
R[6,2] <- 0
R[6,3] <- 0
R[6,4] <- 0
R[6,5] <- 0
}"
#R <- matrix(0, 6, 6, byrow = TRUE)
# Imputation Method 1
big_mat_a <- matrix(0,365,6, byrow = TRUE)
for( i in 1:365) {for(j in 1:6){
  big_mat_a[i,j] <- ifelse(is.na(big_mat[i,j]), Y3[i], big_mat[i,j])
  index <- i - 1
  big_mat_a[i,j] <- ifelse(is.na(big_mat_a[i,j]), big_mat_a[index-1, j], big_mat_a[i,j])
}}
summary(big_mat_a)


# Create data list --------------------------------------------------------

data_list <- list(days = days, pixels = pixels, response = big_mat_a)

init   <- list(taue=1)
model1 <- jags.model(textConnection(model_test),inits=init,data = data_list, n.chains=1)

