## Assignment 1 ##
## Author: Mimoza Dani
## Date: 01.12.2022

## Owl model ##
# 4.c
r <- 0.86
K <- 266
Nt <- 186

owl <- function (Nt, r, K){
  Nt1 <- Nt + r * Nt * (1-(Nt/K))
  return(Nt1)
}
Nt1 <- owl(Nt, r, K)
N <- rep(NA, 20)
N[1] <- Nt
for(i in 1:20){
  N[i] <- owl(Nt, r, K)
  Nt<- N[i]
}
plot(N, xlab='time', ylab='N', pch=19, col="black")

#5
#Step 1. Build the transition matrix A
P.12 <- 0.344
P.13 <- 0
P.22 <- 0.767
P.23 <- 0.1
P.32 <- 0
P.33 <- 0.1
F1 <- 0.304
F2 <- 0.304
F3 <- 0.304
Nt <- .7*K

A <- matrix(c(
  F1, F2, F3,
  P.12, P.13, P.22, P.23, P.32, P.33)
  ,nrow=3,ncol=3,byrow=T)

A
# Step 2. Build the matrix Nt (initial abundance matrix)
Nt <- c((Nt/3),(Nt/3), (Nt/3))

# Step 3. Matrix multiplication for Nt+1
Year1 <- A %*% Nt

# matrix multiplication!
Year1
# Step 4. Create a new variable to hold future values of N
N <- array(NA,dim=c(20,3))

# Step 5. Create a for loop to iteratively calculate N
N[1,] <- Nt

for(i in 2:20){
  N[i,] <- A %*% N[i-1,]
}

