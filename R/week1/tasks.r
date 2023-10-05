# x <- c(1,2,3) <=> x = c(1,2,3) - vector
# 1:10 20:5 - range
# seq (1, 10, 2)(start, finish, step) - sequence
# rep(2, 10) - prints 2 10 times
# rm(x) - deletes x
# sapmle(range, size, replace = F) - generates size random elements from range without repetititon
# sapmle(range, size, replace = T) - generates size random elements from range with repetititon
# replicate( times, func) - executes func <times> times and returns vector with the results 
# table(vector) - makes table with the different elements and how many times they are seen in the vector
# sum(vector==value) - returns the count of all the elements with value <value> in the vector
# length(vector)
# duplicated(vector) - returns true for each element if it is duplicated
# any(bool array) - returns true if at least one of the elements is true

# Task 1
sim.balls1 <- function() {
  x <- sample( 1:8, 2, replace = T )
  x[1]==x[2]
}

sim.balls1()

res <- replicate( 100000, sim.balls1() )
res[1:20]
table(res)
sum(res==T) / length(res)

# Task 2
sim.balls2 <- function() {
  socks <- c(1,1,2,2,3,3)
  x <- sample(socks, 2, replace=F)
  x[1]==x[2]
}

prob.socks <- function(n) {
  res <- replicate( n, sim.balls2())
  sum(res==T) / length(res)
}

prob.socks(100000)

# Task 3
sim.keys <- function(target_key) {
  x <- sample( 1:4, 4, replace=F )
  x[4]==target_key
}

prob.keys <- function(n) {
  res <- replicate(n, sim.keys(1))
  sum(res)/length(res)
}

prob.keys(100000)

# Task 4
sim.questions <- function() {
  questions <- c( rep(0, 3), rep(1, 17))
  x <- sample(questions, 2, replace = F)
  sum(x) == 1
}

prob.questions <- function(n) {
  res <- replicate(n, sim.questions())
  sum(res)/length(res)
} 

prob.questions(100000)

# Task 5
sim.bdays <- function(k) {
  x <- sample( 1:365, k, replace=T)
  any(duplicated(x))
}

prob.bdays <- function(n, k) {
  res <- replicate(n, sim.bdays(k))
  sum(res)/length(res)
}

prob.bdays(100000, 25)

# Task 6
sim.colleagues <- function(k) {
  x <- sample(1:k, k, replace=F)
  any((x - c(1:k))==0)
}

prob.colleagues <- function(n, k) {
  res <- replicate(n, sim.colleagues(k))
  sum(res)/length(res)
}

prob.colleagues(100000, 20)
