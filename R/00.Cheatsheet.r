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



## Distribution Functions

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