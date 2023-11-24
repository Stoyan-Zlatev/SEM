# Task 64

# X_1, ..., X_{66}
# E(X_i) = \mu
# \sigma = sqrt(Var(X_i)) = 4.1
# H_0: \mu = 60
# H_1: \mu != 60
# Xbar = 1/n(X_1 + ... + X_n)
# Z = (Xbar - \mu) / (\sigma * sqrt(n)); Z ~ N(0,1) за големи n
# Ако |Z| >= 1.96 отхвърляме H_0
# Ако |Z| < 1.96 не отхвърляме H_0

# P(Z <= -1.96 или Z >= 1.96) = 0.05
# P(отхърляме H_0 | H_0 е вярна) = 0.05

X.bar <- 61.9
sigma <- 4.1
n <- 66
mu <- 60

z.obs <- (X.bar - mu) / (sigma/sqrt(n))
z.obs > 1.96
# => Отхвърляме H_0 => \mu!=60

# P(Z <= -|z_{obs}|) + P(Z >= |z_{obs}|) = 2P(Z <= -|Z_{obs}|)

p.value <- 2*(1-pnorm(abs(z.obs)))



# Task 65

# p = P(гълъб намира пътя)
# H_0 : p = 0.51
# H_1 : p > 0.51

# X_i ~ Ber(p)   -> 1 if found else 0
# X = X_1 + ... + X_n ~ Bi(n=58,p)
# E(X_i) = p
# Var(X_i) = p(1-p)

# Z = (X/n - p) / (p*(1-p) / n)  ~ N(0,1) за големи N

# z_{obs} = Z = (X/n - p) / (p*(1-p) / n) където x=32, n=58    obs<=>observed

# p.value = P(Z >= z_{obs}) = 1 - pnorm(z.obs)
# Ако p.value <= 0.05   => отхвърляме H_0 в полза на H_1


x <- 32
n <- 58
p <- 0.51

z.obs <- (x/n - p) / sqrt(p*(1-p)/n)

p.value <- 1-pnorm(z.obs)
p.value

p.value > 0.05
# Няма основание да отхвърлим H_0  => вероятността не е над 0,51

prop.test(x = 32, n = 58, p = 0.51, alternative = "greater",correct = F)
prop.test(x = 32, n = 58, p = 0.51, alternative = "greater",correct = F)$p.value



# Task 66

# X_1, ... , X_n
# \mu = E(X_i)
# H_0 : \mu = 4
# H_1 : \mu < 4
# x.bar = 1/n (X_1 + ... + X_n)
# s = sqrt(1/(n-1) \sum_i (X_i - x.bar)^2 )    -> std div
# T = (x.bar - \mu) / (s / \sqrt(n)) има t-разпределение с df=n-1 степени на свобода

# t_{obs} = (x.bar - \mu) / (s / \sqrt(n)) -> наблюдение с конкретни данни 
# p.value = P(T <= t_{obs}) = pt(t.obs, df=n-1)


curve(dnorm(x,0,1),from=-4,to=4, lwd=2)
curve(dt(x,df=50),add=T,lty="longdash",col="red",lwd=2)
# df->\inf => t ~ norm

x <- c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)
x.bar <- mean(x)
s <- sd(x)
n <- length(x)
mu <- 4

t.obs <- (x.bar - mu) / (s/sqrt(n))
p.value <- pt(t.obs, df=n-1)

p.value < 0.05
# Отхвърляме H_0 => средното е < 4

t.test(x, mu=4, alternative = "less")


# Task 67

#a
# X_i = ниво на хемоглобин на i-тото дете
# E(x_i) = \mu
# i = 1, ... ,10
# H_0: \mu = 14.6
# H_1: \mu != 14.6
# p.value = P(T <= -|t_{obs}|) + P(T >= |t_{obs}|) = 
#         = 2 * (1-pt(abs(t.obs), df=n-1))
#         = 2 * (1-pt(-abs(t.obs), df=n-1))


x <- c(12.3) # TODO: complete the vector
mu <- 14.6
n <- length(x)
x.bar <- mean(x)
s <- sd(x)

t.obs <- (x.bar-mu)/(s/sqrt(n))
p.value <- 2*(pt(-abs(t.obs),df=n-1))
p.value

p.value > 0.05
# Не отхвърляме H_0 => не е различно от 14,6

t.test(x,mu=14.6)
t.test(x,mu=14.6, alternative="two.sided")

#b
# X_i = ниво на хемоглобин на i-тото дете
# E(x_i) = \mu
# i = 1, ... ,10
# H_0: \mu = 14.6
# H_1: \mu < 14.6
# p.value = P(T < t_{obs}) 

p.value <- pt(t.obs, df=n-1)

t.test(x,mu=14.6, alternative="less")

p.value < 0.05
# Отхвърляме H_0 => има основания средното да е < 14.6



# Task 68

#a
# p = вероятността стока да е с грешна цена
# H_0 : p = 0.075
# H_1 : p != 0.075
# p.value = P(Z <= -|z_{obs}|) + P(Z >= |z_{obs}|) = 
#         = 2 * pnorm(-abs(z.obs))
x <- 14
n <- 200
p <- 0.075

z.obs <- (x/n - p)/sqrt(p*(1-p)/n)

p.value <- 2 * pnorm(-abs(z.obs))
p.value > 0.05

# Не отхвърляме H_0

prop.test(x=14, n=200, p=0.075, correct=F)

prop.test(x=14, n=200, p=0.075, correct=F)$p.value


#b
# p = вероятността стока да е с грешна цена
# H_0 : p = 0.075
# H_1 : p < 0.075
# p.value = P(Z < z_{obs}) = pnorm(z.obs)

p.value <- pnorm(z.obs)
p.value > 0.05

# Нямаме основание да отхвърлим H_0


prop.test(x=14, n=200, p=0.075, correct=F, alternative="less")



# Task 69
# X_1, ... , X_n
# \mu = E(X_i)
# \sigma = \sqrt(Var(X_i)) = 3.9
# H_0 : \mu = 170
# H_1 : \mu < 170
# x.bar = 168
# z_{obs} = ( x.bar - \mu ) / (\sigma / \sqrt(n))
# p.value = P(Z < z_{obs}) = pnorm(z.obs)

x.bar <- 168
sigma <- 3.9
n <- 50
mu <- 170

z.obs <- (x.bar - mu) / (sigma/sqrt(n))
p.value <- pnorm(z.obs)

p.value < 0.05

#Отхъвърляме H_0 => средното е < 170
