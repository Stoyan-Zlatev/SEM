# Task 32
x1 <- runif(500,2,3)
hist(x1, probability = T)
curve(dunif(x,2,3),from=2, to=3, add=T, lwd=3)

# Task 33
x2 <- rexp(500,1/7)
hist(x2,probability=T,breaks=seq(0,max(x2)+10,2))
curve(dexp(x,1/7),from=0,to=max(x2), add=T, lwd=3)

# Task 34
x3 <- rnorm(500,0,1)
hist(x3,probability=T)
curve(dnorm(x,0,1),add=T, lwd=3)


# Task 35
# X = количество душ гел в опаковка
# X ~ U(248,255)
# P(X <= 250)
punif(250,248,255)

# v=? P(X>v) = 0.95
# P(X <= v) = 0.05
qunif(0.05,248,255)



# Task 36
# X = време на живот
# X ~ Exp(1/10)
# P(X > 10) = 1 - P(X <= 10)
1 - pexp(10,1/10)

# P(7 < X < 11) = P(X < 11) - P(X < 7)
pexp(11,1/10) - pexp(7,1/10)

# t=? P(X > t) = 0.97
# P(x <= t) = 0.03
qexp(0.03,1/10)



# Task 37
# X = изразходвано количество
# X ~ N(mu=41, sigma=5)
# P(X > 51) = 1 - P(X <= 51)
1 - pnorm(51,41,5)

# P(45 < X < 50) = P(X < 50) - P(X < 45)
pnorm(50,41,5) - pnorm(45,41,5)

# t=? P(X <= t) = 0.99
qnorm(0.99,41,5)



# Task 38

# simulations

# P(A)
vol <- rnorm(100000,252,3)
sum(vol>250)/length(vol)

# P(B)
sim.vol <- function(){
  cups<-rnorm(5,252,3)
  sum(cups>250) <= 2
}

prob.vol <- function(n){
  res <- replicate(n, sim.vol())
  sum(res)/length(res)
}

prob.vol(100000)


# distribution functions

#P(A)
# X = обем течност
# X ~ N(mu=252, sigma=3)
# P(X > 250) = 1 - P(X <= 250)
p.a <- 1-pnorm(250,252,3)

#P(B)
# Y = брой наливания с над 250 от общо 5 наливания
# Y ~ Bi(n=5, p=P(A))
# P(Y <= 2)
pbinom(2,5,p.a)



# Task 39
n <- 10^7
x <- runif(n,-1,1)
y <- runif(n,-1,1)
estim.pi <- 4*sum(x^2 + y^2 < 1)/n
estim.pi
abs(estim.pi-pi)
