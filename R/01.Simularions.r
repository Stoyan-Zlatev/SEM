# Task 1
sim.balls1 <- function() {
  x <- sample(1:8, 2, replace = T)
  x[1] == x[2]
}

sim.balls1()

res <- replicate(100000, sim.balls1())
res[1:20]
table(res)
sum(res == T) / length(res)

# Task 2
sim.socks <- function() {
  socks <- c(1, 1, 2, 2, 3, 3)
  x <- sample(socks, 2, replace = F)
  x[1] == x[2]
}

prob.socks <- function(n) {
  res <- replicate(n, sim.socks())
  sum(res == T) / length(res)
}

prob.socks(100000)

# Task 3
sim.keys <- function(target_key) {
  x <- sample(1:4, 4, replace = F)
  x[4] == target_key
}

prob.keys <- function(n) {
  res <- replicate(n, sim.keys(1))
  sum(res) / length(res)
}

prob.keys(100000)

# Task 4
sim.questions <- function() {
  questions <- c(rep(0, 3), rep(1, 17))
  x <- sample(questions, 2, replace = F)
  sum(x) == 1
}

prob.questions <- function(n) {
  res <- replicate(n, sim.questions())
  sum(res) / length(res)
}

prob.questions(100000)

# Task 5
sim.bdays <- function(k) {
  x <- sample(1:365, k, replace = T)
  any(duplicated(x))
}

prob.bdays <- function(n, k) {
  res <- replicate(n, sim.bdays(k))
  sum(res) / length(res)
}

prob.bdays(100000, 25)

# Task 6
sim.colleagues <- function(k) {
  x <- sample(1:k, k, replace = F)
  any((x - c(1:k)) == 0)
}

prob.colleagues <- function(n, k) {
  res <- replicate(n, sim.colleagues(k))
  sum(res) / length(res)
}

prob.colleagues(100000, 20)


#Task 7

sim.ant <- function() {
  a1 <- sample(c(2, 3), 1)
  a2 <- sample(c(1, 3), 1)
  a3 <- sample(c(2, 1), 1)
  any(duplicated(c(a1, a2, a3))) == F
  # or length(unique(c(a1,a2,a3))) == 3
  #or all(c(1,2,3) %in% c(a1,a2,a3))
}

prob.ant <- function(n) {
  res <- replicate(n, sim.ant())
  sum(res) / length(res)
}

prob.ant(100000)


#Task 8

sim.egg <- function() {
  eggs <- c(rep("b", 2), rep("r", 6))
  draws <- sample(eggs, 8, replace = F) # generate permutation
  player1 <- draws[seq(1, 7, 2)]
  player2 <- draws[seq(2, 8, 2)]
  c(sum(player1 == "b"), sum(player2 == "b"))
}

prob.egg <- function(n) {
  res <- replicate(n, sim.egg())
  C <- sum(res[1,] == 2) / n
  D <- sum(res[2,] == 2) / n
  A <- C + D
  B <- sum(res[1,] == 1) / n
  c(A, B, C, D)
}

prob.egg(100000)


#Task 9

sim.exam <- function() {
  sum(sample(c(0, 0, 0, 1), 10, replace = T))
  # or sum(sample(c(0,1), 10, replace=T, prob=c(0.75,0.25)))
}

prob.exam <- function(n, m) {
  res <- replicate(n, sim.exam())
  sum(res >= m) / length(res)
}

prob.exam(100000, 5)


#Task 10

sim.passangers <- function() {
  sum(sample(c(0, 1), 143, replace = T, prob = c(0.08, 0.92)))
}

prob.passangers <- function(n, kMin, kMax) {
  res <- replicate(n, sim.passangers())
  sum(res >= kMin & res <= kMax) / length(res)
}

prob.passangers(100000, 0, 138)
prob.passangers(100000, 137, 137)


#Task 11

sim.dice.ball <- function() {
  dice <- sample(c(1:6), 1)
  if (dice == 6) {
    ball <- sample(c("g", "g", "r", "r"), 1)
  } else {
    ball <- sample(c("g", "r", "r", "r", "r"), 1)
  }
  c(dice, ball)
}

prob.dice.ball <- function(n) {
  res <- replicate(n, sim.dice.ball())
  a <- sum(res[2,] == "g") / length(res[2,])
  b <- sum(res[1,] != 6 & res[2,] == "g") / sum(res[2,] == "g")
  c(a, b)
}

prob.dice.ball(100000)


#Task 12
sim.money <- function(){
    sample(c(11,11,22,12,21),1)
}

prob.money <- function(n){
    res <- replicate(n, sim.money())
    a <- sum(res == 11 | res == 21) / length(res)
    b <- sum(res == 21) / length(res == 11 | res == 21)
    c(a,b)
}

prob.money(100000)

#Task 13
sim.cards.white <- function() {
  sample(c("w1", "w2", "b1", "b2", "mw", "mb"), 1, replace = F)
}

prob.cards.white <- function(n) {
  res <- replicate(n, sim.cards.white())
  sum(res == "w1" | res == "w2") / sum(res == "w1" | res == "w2" | res == "mw")
}

prob.cards.white(100000)

#Task 14
sim.box.balls <- function() {
  draw <- sample(1:99, 4, replace = F)
  max(draw) == draw[1]
}

prob.box.balls <- function(n) {
  res <- replicate(n, sim.box.balls())
  sum(res) / length(res)
}

prob.box.balls(100000)

#Task 15
sim.people <- function() {
  x <- sample(c(rep(1, 2), rep(0, 18)), replace = F)
  res <- which(x == 1)
  res[2] - res[1] == 1
}

prob.people <- function(n) {
  res <- replicate(n, sim.people())
  sum(res) / length(res)
}

prob.people(100000)

#Task 16
sim.cards <- function() {
  cards <- sample(c(rep(1, 4), rep(0, 48)), 52, replace = F)
  sum(cards[1:13]) == 1 & sum(cards[14:26]) == 1 & sum(cards[27:39]) == 1 & sum(cards[40:52]) == 1
}

prob.cards <- function(n) {
  res <- replicate(n, sim.cards())
  sum(res) / length(res)
}

prob.cards(100000)

#Task 17

sim.floor <- function() {
  floors <- c(2:16)
  me <- sample(floors, 1)
  others <- sample(floors, 6, replace=TRUE)
  c(sum(duplicated(c(me, others))==TRUE) >= 1, length(which(others == me))>=2)
}

prob.floor <- function(n) {
  res <- replicate(n, sim.floor())
  c(sum(res[1,]) / length(res[1,]),
  sum(res[2,]) / length(res[2,]))
}

prob.floor(100000)
