# Task 54
gen <- function(n){
  replicate(100000, sum(rexp(n,1/5)))
}

data <- c(3,7,10,30,90,200)
rs <- sapply(data, gen)
colnames(rs) <- as.character(data)
sapply(1:6, function(i) hist(rs[,i], xlab="",main=paste("Sum of Exp, n =", colnames(rs)[i])))



# Task 55 
gen <- function(n){
  replicate(100000, mean(rexp(n,1/5)))
}

data <- c(3,7,10,30,90,200)
rs <- sapply(data, gen)
colnames(rs) <- as.character(data)
sapply(1:6, function(i) hist(rs[,i], xlab="",main=paste("Mean of Exp, n =", colnames(rs)[i])))



# Task 56
gen <- function(n){
  replicate(100000, mean(runif(n,2,8)))
}

data <- c(3,7,10,30,90,200)
rs <- sapply(data, gen)
colnames(rs) <- as.character(data)
sapply(1:6, function(i) hist(rs[,i], xlab="",main=paste("Mean of Unif, n =", colnames(rs)[i])))



# Task 57

# X_1, ..., X_{100} - времената на живот на всяка крушка
# X_i ~ Exp(\lambda = 1/900)
# \mu = E(X_i) = 1/\lamda = 900
# \sigma = \sqrt{Var(X_i)} = 1/\lambda = 900
# Xbar = 1/100 * (X_1 + ... + X_{100})
# P(Xbar > 980) = P( ((Xbar - 900)/(900/sqrt(100))) > ((980-900)/(900/sqrt(100))) )
#    = 1-pnorm((980-900)/(900/sqrt(100)))

a <- (980-900)/(900/sqrt(100))
1-pnorm(a)

# simulations:
mean.vals <- replicate(100000, mean(rexp(100,1/900)))
sum(mean.vals > 980) / length(mean.vals)



# Task 58

# X_1, ..., X_{50} - времената на чакане
# X_i ~ U(0, 60)
# \mu = E(X_i) = 30
# \sigma = \sqrt{Var(X_i)} = \sqrt((60-0)^2 / 12)
# Xbar = 1/50 * (X_1 + ... + X_{50})
# P(25 < Xbar < 35) = P(Xbar < 35) - P(Xbar < 25) =
#    = P( ((Xbar - 30)/(60/sqrt(50 * 12))) < ((35-30)/(60/sqrt(50 * 12))) ) - P( ((Xbar - 30)/(60/sqrt(50 * 12))) < ((25-30)/(60/sqrt(50 * 12))) )
#    = pnorm((35-30)/(60/sqrt(50 * 12))) - ((25-30)/(60/sqrt(50 * 12)))

a <- (35-30)/(60/sqrt(50 * 12))
b <- (25-30)/(60/sqrt(50 * 12))
pnorm(a) - pnorm(b)

# simulations:
mean.vals <- replicate(100000, mean(runif(50,0,60)))
sum(mean.vals > 25 & mean.vals < 35) / length(mean.vals)



# Task 59

# X_1, ..., X_{49} - времената на чакане
# Xbar = 1/49 * (X_1 + ... + X_{49})
# P(Xbar > 5.5) = 1 - P(Xbar <= 5.5) =
#    = 1 - P(((Xbar-\mu)/(\sigma / \sqrt(n))) <= a ) = 1 - pnorm(a)

x <- c(4:7)
p <- c(0.2, 0.4, 0.3, 0.1)
mu <- sum(x*p)
sigma <- sqrt( sum(x^2 * p) - mu^2)
a <- (5.5 - mu)/(sigma/sqrt(49))
1-pnorm(a)

# simulations:
mean.vals <- replicate(100000, mean(sample(x,49,replace=T, prob=p)))
sum(mean.vals > 5.5) / length(mean.vals)



# Task 60

# X_1, ..., X_{160} - времената на чакане
# \mu = E(X_i) = 24
# \sigma = \sqrt{Var(X_i)} = 7
# P(X_1 + ... + X_{160} > 4000) = 1 - P(X_1 + ... + X_{160} <= 4000) =
#    = 1 - P(((X_1 + ... + X_n - n\mu)/(\sigma * \sqrt(n))) <= a ) = 1 - pnorm(a)

a <- (4000 - 160*24)/(7*sqrt(160))
1-pnorm(a)