## General

# x <- c(1,2,3) <=> x = c(1,2,3) - vector
# 1:10 20:5 - range
# seq (1, 10, 2) - sequence (start, finish, step)
# rep(2, 10) - prints 2 10 times
# rm(x) - deletes x



## Simulations

# sample(range, size, replace = F) - generates size random elements from range without repetitions
# sample(vector, size, replace = T) - generates size random elements choosing from vector with repetitions
# sample(vector, size, replace = T, prob = vector) - generates elements according to probability vector with same length
# replicate( times, func) - executes func <times> times and returns vector with the results 
# table(vector) - makes table with the different elements and how many times they are seen in the vector
# sum(vector==value) - returns the sim of all the elements with value <value> in the vector
# length(vector) - number of elements
# duplicated(vector) - returns true for each element if it is duplicated in the vector
# any(bool array) - returns true if at least one of the elements is true
# unique(vector) - returns vector with the unique elements of the vector
# all(bool array) - returns true if all the elements are true
# c(vector1, vector2) - concatenates the two vectors
# c(1,2,3) %in% c(1,2,3) - returns true/false for each element if the elements of the first vector are in the second vector
# which(vector == value) - returns the indexes where elements are equal to value



## Probability Functions

# X = # of successes in n trials
# p = probability of success in 1 trial
# X ~ Bi(n,p)
# P(X = k) = dbinom(k,n,p)
# P(X <= k) = pbinom(k,n,p)
 
# X = # trials until 1st success
# p = probability of success in 1 trial
# X ~ Ge(p)
# P(X = k) = dgeom(k-1,p)
# P(X <= k) = pgeom(k-1,p)
 
# X = # trials until rth success
# p = probability of success in 1 trial
# X ~ NB(r,p)
# P(X = k) = dnbinom(k-r,r,p)
# P(X <= k) = pnbinom(k-r,r,p)



## Distribution functions

# X - непрекъсната случайна величина
# P(a <= X <= b) = \Integral_a^b f(x) dx = F(b) - F(a)
# f(x) - плътност на X
# P(X <= t) = \Integral_{-\Inf}^t f(x) dx = F(t) - функция на разпределение
# P(X = a) = 0
# P(X < a) = P(X <= a)
# E(X) = \mu = \Integral_{-\Inf}^{Inf} xf(x) dx - средна стойност
# Var(X) = \sigma^2 = E(X-\mu)^2 - дисперсия
# \sigma - стандратно отклонение (колко далеч може да отиват стойностите спрямо средната стойност) 

# X ~ U(a,b) - равномерно разпределение
# f(X) = 1 / (b-a) за x \in [a,b]
# dunif(x,a,b) = f(x)
# punif(q,a,b) = P(X<=q)=F(q)
# qunif(p,a,b) = F^{-1}(p)
# runif(N,a,b) - генерира N равномерно разпределени величини

# X ~ Exp(\lambda) - експоненциалнво разпределение
# f(X) = \lambda e^{\lambda} за x>=0
# dexp(x,lambda) = f(x)
# pexp(q,lambda) = P(X<=q)=F(q)
# qexp(p,lambda) = F^{-1}(p)
# rexp(N,lambda) - генерира N равномерно разпределени величини

# X ~ N(\mu, \sigma^2) - нормално разпределение
# f(X) = ....
# dnorm(x,mu,sigma) = f(x)
# pnorm(q,mu,sigma) = P(X<=q)=F(q)
# qnorm(p,mu,sigma) = F^{-1}(p)
# rnorm(N,mu,sigma) - генерира N равномерно разпределени величини

# hist(vecor) - хистограма
# curve(function,from=START, to=END, add=T(TO PREV GRAPH), lwd=3(THICKNESS))




## Statistics

# library(MASS) - load library
# data(survey) - load specific piece of data
# ?survey - info about the survey
# View(survey) - interactive view of the table
# table(survey$Exer) - table for variable
# attach(survey) - load vars from the data piece without the need to specify the data piece every time
# barplot(<table>) - visualize table
# pie(<table>) - visualize table
# cut(<table>, breaks=seq(30,110,10)) - replace each element with the corresponding interval
# stripchart(<var>, method="stack", pch=<symbol>) - hist-like stacks for each value

# par(mfrow=c(n,m)) # shown n rows and m columns of graphs

# xbar = mean(x)
# Mehat= median(x)
# p-quantile = quantile(x,p)
# IQR = IQR(x) - (075-quantile - 0.25-quantile)
# s = sd(x)   - std.div
# summary(<data>) -  above data calculations summary 
# boxplot(<var>, horizontal=T) - vizualization of yhe above summary (without mean)

# getwd() - current working directory
# load("<file>") - load file from the working directory
