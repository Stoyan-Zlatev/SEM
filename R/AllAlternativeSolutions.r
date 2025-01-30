# task_01

task1.experiment = function(n){
  balls = 1:n
  first_ball = sample(balls, 1, replace = T)
  second_ball = sample(balls, 1, replace = T)
  first_ball==second_ball
}

task1.probability = function(Nrep){
  output = replicate(Nrep, task1.experiment(8))
  sum(output)/Nrep
}

task1.probability(10000)

task1.experiment2 = function(n){
  balls = sample(1:n, 2, replace = T)
  balls[1] == balls[2]
}

task1.probability2 = function(Nrep){
  output = replicate(Nrep, task1.experiment2(8))
  sum(output)/Nrep
}

task1.probability2(10000)


# task_02
task2.experiment = function(n){
  all_socks = rep(1:n, each=2)
  socks = sample(all_socks, 2, replace = F)
  socks[1] == socks[2]
} 

task2.prob = function(Nrep){
  output = replicate(Nrep, task2.experiment(3))
  sum(output) / Nrep
}

task2.prob(10000)


# task_03
task3.experiment = function(n){
  keys = sample(1:n, n, replace = F)
  keys[n] == n
}

task3.prob = function(Nrep){
  output = replicate(Nrep, task3.experiment(4))
  sum(output) / Nrep
}

task3.prob(10000)


# task_04
task4.experiment = function(){
  questions = sample(seq(20)>3, 2, replace=F)
  questions[1] != questions[2]
}

task4.prob = function(Nrep){
  output = replicate(Nrep, task4.experiment())
  sum(output) / Nrep
}

task4.prob(10000)
2*(3/20)*(17/19)


# task_05
task5.experiment = function(n){
  birthdays = sample(1:365, n, replace=T)
  anyDuplicated(birthdays) > 0 
  # Faster than `any(duplicated(birthdays))`
}

task5.prob = function(Nrep){
  output = replicate(Nrep, task5.experiment(25))
  sum(output) / Nrep
}

task5.prob(10000)


# task_06
task6.experiment = function(n){
  people = 1:n
  presents=sample(people, n, replace=F)
  any(presents - people == 0)
}

task6.prob = function(Nrep){
  output = replicate(Nrep, task6.experiment(20))
  sum(output) / Nrep
}

task6.prob(10000)


# task_07
task7.experiment = function(){
  first = sample(c(2, 3), 1)
  second = sample(c(1, 3), 1)
  third = sample(c(1, 2), 1)
  ants = c(first, second, third)
  
  length(unique(ants)) == 3
}

output = replicate(10000, task7.experiment())
sum(output) / length(output)

## version2
task7.experiment_v2 = function(){
  moves = sample(c(-1, 1), 3, replace=T)
  sum(moves) %in% c(-3, 3)
}

output = replicate(10000, task7.experiment_v2())
sum(output) / length(output)


# task_08
task8.experiment = function(){
  eggs = sample(seq(8) > 6, 8, replace = F)
  player1 = sum(eggs[seq(1,8,2)])
  player2 = sum(eggs[seq(2,8,2)])
  c(player1, player2)
}

Nrep = 100000
results = replicate(Nrep, task8.experiment())

## a)
(sum(results[1,] == 2) + sum(results[2,] == 2)) / Nrep


## b)
sum(results[1,] == 1) / Nrep

## c)
sum(results[1,] == 2) / Nrep

## d)
sum(results[2,] == 2) / Nrep

# task_09
task9.experiment = function(n){
  answers = sample(c(T,F,F,F), n, replace = T)
  sum(answers) >= 5
}

output = replicate(10000, task9.experiment(10))
sum(output) / length(output)

## Alt: sample(c(0,1), 10, replace = T, prob = c(0.75, 0.25))
1 - pbinom(4, 10, 1/4)


# task_10
task10.experiment = function(){
  passengers = sample(c(1,0), 143, replace = T, prob=c(0.92, 0.08))
  sum(passengers)
}

output = replicate(10000, task10.experiment())
sum(output <= 138) / length(output)
sum(output == 137) / length(output)

## Alternatively
pbinom(138, 143, 0.92)
dbinom(137, 143, 0.92)


# task_11
## a)
task11.experiment.a = function(){
  box1=c(T,T,F,F)
  box2=c(T,F,F,F,F)
  dice=sample(1:6, 1)
  
  ifelse(dice==6, sample(box1, 1), sample(box2, 1))
}
output = replicate(10000, task11.experiment.a())
sum(output) / length(output)

1/6*1/2 + 1/5*5/6

## b)
task11.experiment.b = function(){
  box1=c(T,T,F,F)
  box2=c(T,F,F,F,F)
  dice=sample(1:6, 1)
  
  ball= ifelse(dice==6, sample(box1, 1), sample(box2, 1))
  c(dice, ball)
}
output = replicate(10000, task11.experiment.b())
sum(output[2,] == T & output[1,] != 6) / sum(output[2,] == T)

(1/5*5/6)/(1/4)


# task_12
## a)
task12.experiment = function(){
  coin11 = c(T,T)
  coin22 = c(F,F)
  coin12 = c(T,F)
  
  box = rbind(coin11, coin11, coin22, coin12, coin12)
  coin = sample(1:5, 1)
  side = sample(1:2, 1)
  
  box[coin, side] 
}

output = replicate(10000, task12.experiment())
sum(output) / length(output)

2/5 * 1 + 1/5 * 0 + 2/5 * 1/2

## b)
task12.experiment.b = function(){
  coin11 = c(T,T)
  coin22 = c(F,F)
  coin12 = c(T,F)
  
  box = rbind(coin11, coin11, coin22, coin12, coin12)
  coin = sample(1:5, 1)
  side = sample(1:2, 1)
  
  c(box[coin, side], box[coin, 3 - side])
}

output = replicate(10000, task12.experiment.b())
sum(output[1, ] == T & output[2, ] == F) / sum(output[1, ] == T)


# task_13

task13.experiment = function(){
  card11 = c(T,T)
  card22 = c(F,F)
  card12 = c(T,F)
  
  box = rbind(card11, card22, card12)
  
  i = sample(1:3, 1)
  side = sample(1:2, 1)
  c(box[i, side], box[i, 3 - side])
}

output = replicate(10000, task13.experiment())
sum(output[1, ] == T & output[2, ] == T) / sum(output[1, ] == T)

1/3 * 1 + 1/3 * 0 + 1/3 * 1/2

2/3


# task_14
task14.experiment = function(){
  balls = sample(1:99, 4, replace = F)
  all(balls <= balls[1])
}

output = replicate(10000, task14.experiment())
sum(output) / length(output)


# task_15
task15.experiment = function(){
  people = sample(1:20, 20, replace = F)
  positions = which(people == 1 | people == 2)
  
  abs(positions[1] - positions[2]) == 1
}

output = replicate(10000, task15.experiment())
sum(output) / length(output)


## task_16
task16.experiment = function(){
  cards = rep(1:13, each = 4)
  players = matrix(sample(cards, 52, replace = F), nrow = 4)
  for (i in 1:4) {
    if (sum(players[i,] == 13) != 1) {
      return(FALSE)
    }
  }
  return(TRUE) 
}

output = replicate(100000, task16.experiment())
sum(output) / length(output) # 0.10501 is correct


# task_17
## a)
task17.experiment.a = function(){
  floors = sample(2:16, 7, replace = T)
  anyDuplicated(floors) > 0
}

output = replicate(100000, task17.experiment.a())
sum(output) / length(output) # 0.80921

1 - (factorial(15)/factorial(15-7))/15^7

## b)
task17.experiment.b = function(){
  floors = sample(2:16, 7, replace = T)
  sum(floors[1] == floors) > 1
}

output = replicate(100000, task17.experiment.b())
sum(output) / length(output) #  0.33776

## Alternative:
1 - dbinom(0, 6, 1/15) # 0.3389708


# task_18
## a)
dbinom(2, 10, 1/6)
choose(10, 2) * (1/6)^2 * (5/6)^(8)

## b)
pbinom(2, 10, 1/6)

## c)
1 - pbinom(1, 10, 1/6)

## d)
pbinom(8, 10, 1/6) - pbinom(2, 10, 1/6)
sum(dbinom(3:8, 10, 1/6))


# task_19
## a) P(X <= 10)
pgeom(10 - 1, 1/6)

## b) P(X >= 6) == 1 - (X <= 5)
1 - pgeom(5 - 1, 1/6)


# task_20
## P(X <= 20) NBIN(3, 1/6), 3 is the number of successes
pnbinom(20-3, 3, 1/6)


# task_21
task21.experiment = function(){
  batteries = sample(seq(8) <= 5, 2, replace = F)
  all(batteries)
}

task21.prob = function(Nrep){
  output = replicate(Nrep, task21.experiment())
  sum(output) / length(output)
}
task21.prob(10000) # *must be 1 - p for the answer

## X ~ HGEOM(8, 5, 2), P(X = 2)
dhyper(2, 5, 8 - 5, 2)


# task_22
## a) X ~ Poi(1) for 500, P(X <= 2) for 1500
ppois(2, 3)
pbinom(2, 1500, 1/500)

## b)
sum(dbinom(1:3, 1500, 1/500))
sum(dpois(1:3, 3))


# task_23 P(X >= 5)
sum(dbinom(5:10, 10, 1/4))
1 - pbinom(4, 10, 1/4)


# task_24
## a) P(X <= 138)
pbinom(138, 143, 0.92)

## b) P(X == 137)
dbinom(137, 143, 0.92)


# task_25
## a) Geom(0.03), P(X <= 10)
pgeom(10 - 1, 0.03)

## b) Bin(50, 0.03), P(X >= 2)
1 - pbinom(1, 50, 0.03)

# task_26
## P(X >= 2), HGeom(100, 3, 50)
1 - phyper(1, 3, 100 - 3, 50)


# task_27
## P(X >= 2), HGeom(3000, 90, 50)
1 - phyper(1, 90, 3000, 50)


# task_28
## a) Geom(1/10) P(X >= 5)
1 - pgeom(4, 1/10)

## b) NBinom(10, 1/10) P(X >= 50)
1 - pnbinom(49 - 10, 10, 1/10)


# task_29
## Poisson(1) for 90, Poi(4) for 360
## P(X > 3) = 1 - P(X <= 3)
1 - ppois(3, 4)


# task_30
task30.experiment = function(){
  cards = rep(1:4, each = 13)
  choosen = sample(cards, 10, replace = F)
  sum(choosen == 1) >= 2
}

task30.prob = function(Nrep){
  output = replicate(Nrep, task30.experiment())
  sum(output) / Nrep
}
task30.prob(100000)

1 - pbinom(1, 10, 1/4) # wrong? yes!
1 - phyper(1, 13, 52 - 13, 10)


# task_31
task31.experiment = function(){
  cards = rep(1:4, each = 13)
  choosen = sample(cards, 10, replace = T)
  sum(choosen == 1) >= 2
}

task31.prob = function(Nrep){
  output = replicate(Nrep, task31.experiment())
  sum(output) / Nrep
}
task31.prob(100000)

1 - pbinom(1, 10, 1/4) # correct this time! :)


# task_32
u = runif(500, 2, 3)
sum(u<2.5) / 500
hist(u, probability = T)
curve(dunif(x, 2, 3), from = 2, to = 3, add=T, lwd = 1.5, col='blue')


u = runif(5000, 2, 3)
hist(u, probability = T)
curve(dunif(x, 2, 3), from = 2, to = 3, add=T, lwd = 1.5, col='blue')
## duinf - generates f
## add - stacks the plots
## lwd - boldness of the line
## col - colour of the line


# task_33
e = rexp(500, 1/7)
hist(e, probability = T, col = 'lightblue', breaks = 100)
curve(dexp(x, 1/7), from = 0, to = max(e), add = T, lwd = 1.5, col = 'black')

e = rexp(5000, 1/7)
hist(e, probability = T, col = 'lightblue', breaks = 100)
curve(dexp(x, 1/7), from = 0, to = max(e), add = T, lwd = 1.5, col = 'black')


# task_34
n = rnorm(500, 0, 1)
hist(n, probability = T, col = 'lightblue', xlim = c(-3.5, 3.5), breaks = 10)
curve(dnorm(x, 0, 1), from = min(n), to = max(n), add = T, lwd = 1.5)

n = rnorm(50000, 0, 1)
hist(n, probability = T, col = 'lightblue', xlim = c(-3.5, 3.5), breaks = 100)
curve(dnorm(x, 0, 1), from = min(n), to = max(n), add = T, lwd = 1.5)


# task_35 Unif(248, 255)
## a) P(X < 250) = P(X <= 250)
punif(250, 248, 255)

## b) v = ?, P(X >= v) = 0.95 
## P(X <= v ) = 0.05
punif(248.35, 248, 255) # by guessing

qunif(0.05, 248, 255) # !! Real solution


# task_36 Exp(1/10) 
## P(X > 10) 
1 - pexp(10, 1/10)

## P(7 <= X <= 11)
pexp(11, 1/10) - pexp(7, 1/10)

## t = ?, P(X >= t) = 0.97
qexp(0.03, 1/10)
1 - pexp(0.30459, 1/10)


# task_37 Norm(41, 5)
## P(X > 51)
1 - pnorm(51, mean = 41, sd = 5)

## P(45 <= X <= 50)
pnorm(50, 41, 5) - pnorm(45, 41, 5)

## t = ?, P(X <= t) = 0.99
qnorm(0.99, 41, 5)
pnorm(52.63174, 41, 5)


# task_38 Norm(252, 3)
## P(X > 250)
1 - pnorm(250, 252, 3) # 0.7475

## Y <= 2 Y ~ Bin(5, p = P(X>250))
p = 1 - pnorm(250, 252, 3) 
pbinom(2, 5, p) # 0.10616

## With simulations:
## a)
task_38.a = function(Nrep) {
  cups = rnorm(Nrep, 252, 3)
  sum(cups > 250) / Nrep 
}

task_38.a(100000) # 0.74665

## b)
task_38.b.experiment = function(Ncups) {
  cups = rnorm(Ncups, 252, 3)
  choosen = sample(cups, 5, replace = F)
  choosen
  sum(choosen > 250) <= 2
}

task_38.b.prob = function(Ncups, Nrep) {
  output = replicate(Nrep, task_38.b.experiment(Ncups))
  sum(output) / Nrep
}

task_38.b.prob(100000, 1000) # 0.102

## Alternatively:
task_38.b.easier = function() {
  cups = rnorm(5, 252, 3)
  sum(cups > 250) <= 2
}

output = replicate(100000, task_38.b.easier())
sum(output) / length(output) # 0.107


# task_39
## S[circle]/S[square] = in/all
## Pi = 4 * in/all

Nrep = 10^9
x = runif(Nrep, -1, 1)
y = runif(Nrep, -1, 1)

incircle = sum(x^2 + y^2 <= 1)
incircle / Nrep * 4 # 3.141344


# task_40 todo
task_40.get_Pi = function(Nrep){
  x = runif(Nrep, -1, 1)
  y = runif(Nrep, -1, 1)
  
  incircle = sum(x^2 + y^2 <= 1)
  incircle / Nrep * 4
}

task_40.get_E = function(Nrep){
 exp(1) # :D
}

Nrep = 10^4
Pi = task_40.get_Pi(Nrep)
E = task_40.get_E(Nrep)

task_40.get_integral = function(Nrep, Pi, E) {
  u = runif(Nrep, 0.8, 4)
  y = exp(-u^2/2)/sqrt(2*pi)
  sum(y) / Nrep
}

task_40.get_integral(Nrep, Pi, E)

integrand = function(x){exp(-x^2/2)/sqrt(2*pi)}
integrate(integrand, 0.8, 4) # 0.2118237


# task_41  Norm(260, 50)
## P(X > 240)
1 - pnorm(240, 260, 50)

## P(180 < X < 300)
pnorm(300, 260, 50) - pnorm(180, 260, 50)

## t = ?, P(X >= t) = 0.90
qnorm(0.1, 260, 50)
1- pnorm(195.9224, 260, 50)

# task_42  Exp(1/5)
## P(X > 10)
1 - pexp(10, 1/5)

## t = ?, P(x > t) = 0.5
qexp(0.5, 1/5)
pexp(3.465736, 1/5)


# task_43
library(MASS)
data(survey)
?survey

attach(survey)
table(Exer)
summary(Exer)
barplot(table(Exer), legend=T,
        args.legend = list(x="center", inset=0.05))
        
sort( table(Exer), decreasing=T )
table(Exer)/length(Exer)
prop.table( table(Exer) )

barplot( sort( table(Exer), decreasing=T ) )
barplot( prop.table( table(Exer) ) )
pie( table(Exer) )
pie( table(Exer), col=c("lightblue", "purple", "darkblue") )


# task_44
Pulse
table(Pulse)
sort( table(Pulse), decreasing = T)

table(Pulse) / length(Pulse)
prop.table( table(Pulse))

barplot(table(Pulse))
hist(Pulse)
hist(Pulse, breaks = seq(30, 120, 5))
hist(Pulse, breaks = seq(30, 120, 7))


table(Pulse, useNA="ifany")
pulse.cutted = cut(Pulse, breaks=seq(30,110,10))
barplot(table(pulse.cutted))
table(pulse.cutted)


stripchart(Pulse, method="stack", pch=20)
stripchart(Pulse, method="stack", pch=18)
stripchart(Pulse, method="stack", pch="*")

# task_45
Age
table(Age)
sort(table(Age))
barplot(sort(table(Age), decreasing = T))

age.cutted = cut(Age, breaks = seq(16,50,2))
table(age.cutted)
barplot(table(age.cutted))

hist(Age)
hist(table(age.cutted))
prop.table ( table(age.cutted))
barplot(prop.table ( table(age.cutted)))

# task_46
v1 <- rep(4, 30)
v2 <- rep(c(3.5,4.5), 15)
v3 <- rep(c(3,5), 15)
v4 <- rep(c(2:6), 6)
v5 <- rep(c(2,6), 15)

pie(table(v1))
barplot(table(v1))

pie(table(v2))
barplot(table(v2))

pie(table(v3))
barplot(table(v3))

pie(table(v4))
barplot(table(v4))

pie(table(v5))
barplot(table(v5))

median(v1) # v1[mid]
median(v4)

sd(v1) # S = sqrt(Dx)
sd(v4)

mean(v1) # avg(x)
mean(v4)

quantile(v4, 0.25)
quantile(v4, 0.50)
quantile(v4, 0.75)
IQR(v4)
boxplot(v4)

mean(v4) - 2*sd(v4)
mean(v4) + 2*sd(v4)
mean(v4) + 1.5*sd(v4)
quantile(v4, 0.25) - 2.5*IQR(v4)

boxplot(v4)
boxplot(v2)

summary(v1)
summary(v4)

par(mfrow=c(2,3))
stripchart(v1, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
stripchart(v2, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
stripchart(v3, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
stripchart(v4, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
stripchart(v5, pch="*", method="stack", xlim=c(2,6), ylim=c(0,10))
par(mfrow=c(1,1))

# task_47
setwd('C:/Users/Teodor.Kostadinov/UNI/Stat/DataPract')
load("cereals.RData")

cereals
summary(cereals)

my.summary = function(somedata) {
  results = c(median(somedata, na.rm = T),
              mean(somedata, na.rm = T),
              sd(somedata, na.rm = T)
              )
  names(results) = c("Median", "Mean", "StDev")
  results
}

my.summary(cereals$carbo)
my.summary(cereals$carbo)['Median']

my.summary(cereals$sodium)
my.summary(cereals$potass)


# task_48
Pulse
W.Hnd
table(W.Hnd)
table(Pulse)

plot(Pulse ~ W.Hnd)
table(Pulse, W.Hnd)

pulse.cutted = cut(Pulse, seq(30, 110, 10))
prop.table(table(pulse.cutted, W.Hnd))
prop.table(table(pulse.cutted, W.Hnd), 1)
prop.table(table(pulse.cutted, W.Hnd), 2)

barplot(table(pulse.cutted , W.Hnd), legend = T,
        args.legend = list(x="topleft", inset = 0.05))

boxplot(Pulse ~ W.Hnd)
stripchart(Pulse ~ W.Hnd, method = 'stack', vertical = T, pch=20)

my.summary( Pulse[W.Hnd=="Left"] )
my.summary( Pulse[W.Hnd=="Right"] )


# task_49
my.summary(survey['Pulse'])
my.summary(Pulse[survey['Sex']=='Female'])
my.summary(Pulse[Sex=='Female'])

my.summary(Pulse[W.Hnd=='Right'])
my.summary(Pulse[Age <= 25])

my.summary(Pulse[Exer=='Freq'])
my.summary(Pulse[Exer=='Freq' & Smoke=='Never'])

# task_50
table(Pulse, Exer)
prop.table(table(Pulse, Exer))
prop.table(table(Pulse, Exer), 1)
prop.table(table(Pulse, Exer), 2)
boxplot(Pulse ~ Exer)

prop.table(table(pulse.cutted, Exer), 1)

# task_51
table(Smoke)
pie(table(Smoke))
barplot(table(Smoke))

prop.table(table(Smoke))


# task_52
good.summary = function(input_data){
  result = c(median(input_data, na.rm = T), 
             mean(input_data, na.rm = T),
             sd(input_data, na.rm = T)
            )
  names(result) = c("Median", "Mean", "StandDev")
  result
}

good.summary(Age)
good.summary(Age[Smoke != 'Never'])
good.summary(Age[W.Hnd != 'Right'])
good.summary(Age[Pulse >= 70])
good.summary(Age[Exer != 'Never'])


# task_53
Height
table(Height)

min(Height, na.rm = T)
max(Height, na.rm = T)
height.cutted = cut(Height, seq(150, 200, 5))
table(height.cutted)

boxplot(Height)
barplot(table(height.cutted))

plot(Height)
stripchart(Height, method = 'stack', pch=20)


# task_54
n.values = c(3, 7, 10, 30, 90, 200)
Nrep = 10000
lamda = 1/5

par(mfrow=c(2,3))
for(n in n.values){
  hist(replicate(Nrep, sum(rexp(n, lamda))),
       main = paste("Sum of exp n = ", n),
       xlab = "sum"
       )
}

## Alternative solution using sapply instead of for
par(mfrow=c(2,3))
results = sapply(n.values, function(n) replicate(Nrep, sum(rexp(n, lamda))))
colnames(results) = n.values
results

sapply(1:6,   function(i) hist(results[,i],
                               main = paste("Sum of exp n = ", n.values[i]),
                               xlab = "sum")
)

sapply(1:6, function(i) i^2)


# task_55
n.values = c(3, 7, 10, 30, 90, 200)
Nrep = 10000
lamda = 1/5

results = sapply(n.values, function(n) replicate(Nrep, mean(rexp(n, lamda))))
colnames(results) = n.values

par(mfrow=c(2,3))
sapply(1:length(n.values), 
       function(i) hist(results[,i],
                        main = paste("Mean Exp for n = ", n.values[i]),
                        xlab = 'mean')
       )


# task_56
n.values = c(3, 7, 10, 30, 90, 200)
Nrep = 10000
a = 2
b = 8

results = sapply(n.values, function(n) replicate(Nrep, mean(runif(n, a, b))))
colnames(results) = n.values

par(mfrow=c(2,3))
sapply(1:length(n.values), 
       function(i) hist(results[,i],
                        main = paste("Mean Norm for n = ", n.values[i]),
                        xlab = 'mean')
)


# task_57
## X ~ Exp(1/900) EX = 1/a, Dx = 1/a^2 
EX = 900
N = 100
s = EX

bound = 980

normalize = function(num, EX, s, N) {
  (num - EX)/(s/sqrt(N)) # (980-900)/(900/sqrt(100))
}

bound.new = normalize(bound, EX, s, N)
bound.new
1 - pnorm(bound.new) # > 980


# task_58 
## X ~ Unif(0, 60), Ex = (a + b)/ 2, Dx = ((b - a) ^ 2)/12
sqrt(var(runif(10000, 0, 60))) # Finds DX and S

a = 0
b = 60
Ex = (a + b)/ 2
Dx = ((b - a) ^ 2)/12
s = sqrt(Dx)

N = 50
lower = 25
upper = 35

lower.new = normalize(lower, Ex, s, N) # (35-30)/(60/(sqrt(12)*sqrt(50)))
upper.new = normalize(upper, Ex, s, N)
c(lower.new, upper.new)

pnorm(upper.new) - pnorm(lower.new)


# task_59
## x | 4    5   6   7
## p | 0.2 0.4 0.3 0.1

x = c(4,5,6,7)
p = c(0.2, 0.4, 0.3, 0.1)

Ex = sum(x * p)
Ex

Dx = sum(x**2 * p) - Ex**2
Dx

s = sqrt(Dx)
n = 49

lowerbound = 5.5
lowerbound.new = (lowerbound - Ex) / (s/sqrt(n))
lowerbound.new

1 - pnorm(lowerbound.new)

## Alternative solution
mean.vals <- replicate(100000, mean(sample(x, 49, replace=T, prob=p)))
sum(mean.vals > 5.5)/length(mean.vals)


# task_60
## X ~ Norm(24, 7^2)  sum(X) >? bound
n = 160
Ex = 24
s = 7
bound = 4000

bound.new = (bound - n*Ex) / (s*sqrt(n)) # because it is a sum
bound.new

1 - pnorm(bound.new)


# task_61 
## X ~ Poi(5), Ex = Dx = a = 5, 4.5 < avg(x) < 5.5
Ex = 5
s = sqrt(Ex)

n = 80
lower = 4.5
upper = 5.5

new.lower = (lower - Ex)/(s/sqrt(n))
new.upper = (upper - Ex)/(s/sqrt(n))
c(new.lower, new.upper)

pnorm(new.upper) - pnorm(new.lower)

# task_62
n = 200

## a)
1 - dbinom(100, 200, 0.5)

## Alternative:
task62.experiment = function() {
  coin = c(rep(1, each=5), rep(0, each=5))
  sum(sample(coin, 200, replace=T) == 1)
}

task62.prob = function() {
  result = replicate(10000, task62.experiment())
  1 - sum(result == 100)/length(result)
}

task62.prob()

## b)
0.03 * 200
0.05 * 200
a = 14
x = 100

pbinom(x + a, 200, 0.5) - pbinom(x - a, 200, 0.5)
?qbinom
qbinom(0.96, 200, 0.5)

## c)
p = 0.6
pbinom(x + a, 200, p) - pbinom(x - a, 200, p)

## d)
p = 0.7
pbinom(x + a, 200, p) - pbinom(x - a, 200, p)


# task_63
Mu = 6.7
S = 0.12
n = 45
alpha = 0.036

task63.experiment = function () {
  Mu2 = mean(rnorm(n, Mu, S))
  abs(Mu2 - Mu) >= alpha
}

task63.prob = function() {
  result = replicate(10^5, task63.experiment())
  sum(result) / length(result)
}

task63.prob()

## alternative:
a <- 0.036/(0.12/sqrt(45))
2*(1-pnorm(a)) # 0.04417134

## b) -0.036 < x < 0.036
Mu2 = 6.75
b1 = (alpha + Mu - Mu2)/(S/sqrt(n))
b2 = (-alpha + Mu - Mu2)/(S/sqrt(n))
prob.in = pnorm(b1) - pnorm(b2)
1 - prob.in

## c)
Mu2 = 6.8
b1 = (alpha + Mu - Mu2)/(S/sqrt(n))
b2 = (-alpha + Mu - Mu2)/(S/sqrt(n))
prob.in = pnorm(b1) - pnorm(b2)
1 - prob.in

## d)
task63.try = function(alpha) {
  Mu2 = 6.7
  a1 = (alpha + Mu - Mu2)/(S/sqrt(n))
  a2 = (-alpha + Mu - Mu2)/(S/sqrt(n))
  prob.in = pnorm(a1) - pnorm(a2)
  1 - prob.in
}

task63.try(0.035)


# task_64
n = 66
Mu = 61.9
S = 4 # not needed, just as XML :)
Sreal = 4.1
Mreal = 60

z = (-Mreal + Mu) / (Sreal/sqrt(n))
p = 2*(1-pnorm(abs(z)))

p # 0.0001666836, p < 0.05 => Mreal is not 60!


# task_65
all = 58
good = 32

p = 0.51
?prop.test
prop.test(x=good, n=all, p=p, 'greater', correct = FALSE)$p.value

## alternative:
x = 58
p = 0.51
n = 32
z = (x/n - p) / sqrt(p*(1-p))/sqrt(n)
1 - pnorm(z)


# task_66
n = 10
data = c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)
S = sd(data)
Mu = mean(data)
Mu.hyp = 4
S

z = (Mu - Mu.hyp) / (S / sqrt(n))
z

pt(z, n - 1)

## Alternative:
result = t.test(data, mu=Mu.hyp, alternative = 'less')
result$p.value


# task_67
x <- c(12.3, 11.2, 14.2, 15.3, 14.8, 13.5, 11.1, 15.1, 15.4, 13.2)
n = length(x)

# a)
Mu.hyp = 14.6
t.test(x, mu = Mu.hyp)$p.value 
## 0.08773213 is not < so we can't say it is not mu.hyp
## alternative:

z = (mean(x) - Mu.hyp)/(sd(x)/sqrt(n))
2*(1 - pt(abs(z), n-1))

## b)
Mu.hyp = 14.6
t.test(x, mu = Mu.hyp, alternative = 'less')$p.value # yes

pt(z, n-1)


# task_68
p = 0.075
n = 200
x = 14

prop.test(x = x, n = n, p = p, correct = FALSE)$p.value

z = (x/n - p) / sqrt(p*(1-p)/n)
z

2*(1 - pnorm(abs(z)))


## b)
prop.test(x = x, n = n, p = p, alternative = 'less', correct = FALSE)$p.value

pnorm(z)


# task_69
Mu.hyp = 170
s = 3.9
n = 50
Mu = 168

z = (Mu - Mu.hyp) / (s/sqrt(n))
z

pnorm(z) # yes
z.test


# task_70
setwd('C:/Users/Teodor.Kostadinov/UNI/Stat')
examAB = read.table( "DataPract/examAB.txt", header=T )

A = examAB$points[examAB$variant == 'A']
B = examAB$points[examAB$variant == 'B']

t.test(A, B, alternative = 'greater')$p.value
## notice the 'greater', A is the default


# task_71
setwd('C:/Users/Teodor.Kostadinov/UNI/Stat')
reacttime = read.table("DataPract/reacttime.txt", header=T)
Before = reacttime$before
After = reacttime$after
t.test(Before, After, alternative='less', paired=T) # yessir


# task_72
n1 = 200
n2 = 200

x1 = 8
x2 = 15

n = c(n1, n2)
x = c(x1, x2)

prop.test(x, n, correct = F)


# task_73

## A) paired = T
## B) paired = F
## C) paired = T
## D) paired = F


# task_74

task_74.hyp = function(n) {
 x = rnorm(n, 5, 1)
 y = rnorm(n, 5, 0.64)
 t.test(x, y)$p.value >= 0.05
}

task_74.hyp(10)

task_74.prob = function(Nrep, n) {
  results = replicate(Nrep, task_74.hyp(n))
  sum(results) / Nrep
}

task_74.prob(10^4, 10)

task_74.all = function(Nrep) {
  n = c(20, 50, 100, 500)
  sapply(n.values, function(n) task_74.prob(Nrep, n)  )
}

task_74.all(10^4) #reversed


# task_75

task_75.hyp = function(n) {
  x = rnorm(n, 5, 1)
  y = rnorm(n, 5.2, 1)
  t.test(x, y)$p.value >= 0.05
}

task_75.hyp(10)

task_75.prob = function(Nrep, n) {
  results = replicate(Nrep, task_75.hyp(n))
  sum(results) / Nrep
}

task_75.prob(10^4, 10)

task_75.all = function(Nrep) {
  n = c(20, 50, 100, 500, 1000)
  sapply(n.values, function(n) task_75.prob(Nrep, n)  )
}

task_75.all(10^4)


# task_76
n1 = 500
x1 = 26

n2 = 540
x2 = 43

n = c(n1, n2)
x = c(x1, x2)

prop.test(x, n, correct = F, alternative = 'less')


# task_77
x = c(1.2, 1.3, 1.5, 1.4, 1.7, 1.8, 1.4, 1.3)
y = c(1.4, 1.7, 1.5, 1.3, 2.0, 2.1, 1.7, 1.6)

t.test(x, y, paired = T)


# task_78
Mu1 = 7.88
s1 = 1.73

Mu2 = 8.48
s2 = 2.12

n = 50

x = rnorm(n, Mu1, s1)
y = rnorm(n, Mu2, s2)
t.test(x, y, paired = T)


# task_79
## A < (X - Mu) / (s/sqrt(N)) < B
n = 66
x.bar = 61.9
S = 4
Sreal = 4.1

## a)
alpha = 0.05

find.interval = function(x.bar, S, n, alpha) {
  l = x.bar - qnorm(1 - alpha/2) * S/sqrt(n)
  r = x.bar + qnorm(1 - alpha/2) * S/sqrt(n)
  c(l, r)
}

find.interval(x.bar, S, n, alpha)

## b) 
find.interval(x.bar, S, n = 88, alpha)

## c) from task_64
z = (-x.bar + 60) / (S/sqrt(n))
p = 2*(1-pnorm(abs(z)))
p


# task_80
x = c(3.1, 3.0, 3.7, 2.6, 4.2, 3.8, 3.6, 2.7, 3.8, 4.4)

find.t.interval = function(x.bar, S, n, alpha) {
  l = x.bar - qt(1 - alpha/2, df = n - 1) * S/sqrt(n)
  r = x.bar + qt(1 - alpha/2, df = n - 1) * S/sqrt(n)
  c(l, r)
}

## a) b)
find.t.interval(mean(x), sd(x), length(x), 0.05)
find.t.interval(mean(x), sd(x), length(x), 0.10)

## alternative a) b)
t.test(x, conf.level = 0.95)
?t.test

result = t.test(x, conf.level = 0.95)
result$conf.int[1:2]

t.test(x, conf.level = 0.90)$conf.int[1:2]


# task_81
n = 58
x = 32
conf= 0.95

prop.test(x, n, conf.level = conf, correct=F)$conf.int[1:2]
?prop.test

## alternative:
find.p.interval = function(x, n, alpha) {
  p = x/n
  l = p - qnorm(1 - alpha/2) * sqrt(p*(1-p)/n)
  r = p + qnorm(1 - alpha/2) * sqrt(p*(1-p)/n)
  c(l, r)
}

find.p.interval(x, n, 0.05)


# task_82
Nrep = 10^4

task_82.experiment = function(n) {
  data = runif(n, 5, 9)
  interval = t.test(data, conf.level = 0.95)$conf.int[1:2]
  interval[1] <= 7 & interval[2] >= 7
}

task_82.experiment(10)

task_82.prob = function(Nrep, n){
  result = replicate(Nrep, task_82.experiment(n))
  sum(result) / Nrep
}

task_82.prob(Nrep, 100)

task_82.all = function(Nrep) {
  n.values = c(20, 50, 100, 500)
  sapply(n.values, function(n) task_82.prob(Nrep, n))
}

task_82.all(10^5)


# task_83
task_83.experiment = function(n) {
  data = runif(n, 5, 9)
  t.test(data, mu = 7)$p.value >= 0.05
}

task_83.prob = function(Nrep, n){
  result = replicate(Nrep, task_83.experiment(n))
  sum(result) / Nrep
}

task_83.all = function(Nrep) {
  n.values = c(20, 50, 100, 500)
  sapply(n.values, function(n) task_83.prob(Nrep, n))
}

task_83.all(10^5)


# task_84
task_84.experiment = function(n) {
  data = runif(n, 5, 9)
  accept.ho = t.test(data, mu = 7)$p.value >= 0.05
  interval = t.test(data, conf.level = 0.95)$conf.int[1:2]
  mu7.isin = interval[1] <= 7 & interval[2] >= 7
  accept.ho & mu7.isin
}

task_84.prob = function(Nrep, n){
  result = replicate(Nrep, task_84.experiment(n))
  sum(result) / Nrep
}

task_84.all = function(Nrep) {
  n.values = c(20, 50, 100, 500)
  sapply(n.values, function(n) task_84.prob(Nrep, n))
}

task_84.all(10^5)


# task_85
n = 50
conf.level = 0.95
conf,int = c(25.0128, 26.0212)
Mu = 25
alpha = 0.05

?skip


# task_86
counts = c(28, 36, 36, 30, 27, 23)
# x = counts / sum(counts) # no need to do that
probs = rep(1/6, each = 6)
probs

chisq.test(x = counts, p = probs)


# task_87
load("DataPract/letterFreq.RData")
probs
x1 # the counts
chisq.test(x1, p=probs)


# task_88
probs = c(1/4, 1/2, 1/4)
x1 = c(141, 291, 132)

chisq.test(x1, p = probs)


# task_89
load("DataPract/pi2000.RData")
x1 = table(pi2000)
probs = rep(1/10, each = 10)

chisq.test(x1, p = probs)


# task_90
library(MASS)
t = table(survey$Smoke, survey$Sex)
chisq.test(t)
chisq.test(t)$expected


# task_91
eyes = read.table("DataPract/ManWomanEye.txt", header = T)
chisq.test(table(eyes))$expected
chisq.test(table(eyes)) # < so there is a connection


# task_92

data(HairEyeColor)
HairEyeColor[,,1]
HairEyeColor[,,2]
tb = HairEyeColor[,,1] + HairEyeColor[,,2]
tb
chisq.test(tb)



# task_93
data_matrix = matrix(c(12813, 647, 359, 42, 65963, 4000, 2642, 303), nrow = 2, byrow = TRUE)
data_frame = as.data.frame(data_matrix)
colnames(data_frame) = c("Няма", "Леки", "Средни", "Тежки")
rownames(data_frame) = c("С колан", "Без колан")
print(data_frame)

chisq.test(data_frame) # there is


# task_94
n.values = c(100, 200, 400)
Nrep = 10^4

task_94.experiment = function(n){
  dice = c(1,2,3,4,5,6)
  x = sample(dice, n, replace = T)
  probs = rep(1/6, each=6)
  chisq.test(table(x), p=probs)$p.value >= 0.05
}

task_94.prob = function(Nrep, n) {
  results = replicate(Nrep, task_94.experiment(n))
  sum(results) / Nrep
}

task_94.all = function(Nrep, n.values) {
  sapply(n.values, function(n) task_94.prob(Nrep, n))
}

task_94.all(Nrep, n.values)


# task_95

n = 100
x = rnorm(n, 5, 1)
x

par(mfrow = c(2,3))

## a)
y = 2*x
plot(x,y, main = cor(x,y))
cor(x,y)

## b)
ep = rnorm(n, 0, 1)
y = 2*x + ep

plot(x, y, main = cor(x,y))
cor(x,y)

## c)
ep = rnorm(n, 0, 4)
y = 2*x + ep

plot(x, y, main = cor(x,y))
cor(x,y)

## d)
ep = rnorm(n, 0, 1/4)
y = 0.1*x + ep

plot(x, y, main = cor(x,y))
cor(x,y)

## e)
y = rnorm(100, 5, 1)

plot(x, y, main = cor(x,y))
cor(x,y)

## f)
ep = rnorm(n, 0, 1)
y = -2*x + ep

plot(x, y, main = cor(x,y))
cor(x,y)


# task_96


## a)
tt = read.table('DataPract/bac.txt', header = T)
m1 = lm(bac ~ beers, data = tt)
m1

plot(bac ~ beers, data = tt)
abline(coef(m1), lwd = 2)

plot(fitted(m1))
fitted(m1)
residuals(m1)
plot(fitted(m1), residuals(m1))

## b)
coef(m1)

## c)
summary(m1)
summary(m1)$coefficients
p.value = summary(m1)$coefficients[2,4]
p.value < 0.05

## alternative
beta1 = 0
!(confint(m1)[2,1] <= beta1 & beta1 <=confint(m1)[2,2])

## d)
beta1 = 0.02
confint(m1)[2,1] <= beta1 & beta1 <= confint(m1)[2,2]

## e)

?predict
predict(m1, data.frame(beers = c(5)), interval = 'confidence')

## f)
xx <- data.frame( beers=seq(1, 9, 0.5) )
cint = predict( m1, xx, interval="confidence" )
cint

plot( bac ~ beers, data=tt )
abline( coef(m1), lwd=2 )
lines( xx$beers, cint[,2])
lines( xx$beers, cint[,3])

pint = predict( m1, xx, interval="prediction" )
pint
lines( xx$beers, pint[,2], type="l", lty="dotted", col="coral", lwd=2 )
lines( xx$beers, pint[,3], type="l", lty="dotted", col="coral", lwd=2 )


# task_97
set.seed(12)
n = 50
x = runif(n, 1, 7)

## a)
s = 2
eps = rnorm(n, 0, s)

b0 = 2
b1 = 1.5
y = b0 + b1*x + eps
y

m1 = lm(y ~ x)
m1
plot(y ~ x)
abline( coef(m1), lwd=2 )
abline( a=b0, b=b1, col="coral", lwd=2 )

summary(m1)
summary(m1)$r.squared
confint(m1)

## b)
b0 = 2
b1 = 1.5
S = 1
eps = rnorm(n, 0, S)
y = b0 + b1*x + eps

m2 = lm(y ~ x)
m2

plot(y ~ x)
abline(coef(m2), lwd = 2)
abline(a = b0, b = b1, col = 'coral', lwd = 2)

confint(m2)
summary(m2)$r.squared

## c)
b0 = 2
b1 = 0.17
S = 1
eps = rnorm(n, 0, S)
y = b0 + b1*x + eps

m2 = lm(y ~ x)
m2

plot(y ~ x)
abline(coef(m2), lwd = 2)
abline(a = b0, b = b1, col = 'coral', lwd = 2)

confint(m2)
summary(m2)$r.squared


## task_98

## b)
n = 50
x = runif(n, 1, 7)
eps = rnorm(n, 0, 2.5)

b0 = 2
b1 = 1.1
y = b0 + b1*(x^2) + eps

m2 = lm(y ~ x)
m2

plot(y ~ x)
abline(coef(m2), lwd = 2)

confint(m2)
summary(m2)$r.squared

plot(fitted(m2), resid(m2))


# task_99

satgpa = read.table("Datapract/satgpa.txt", header = T)
satgpa

## a)
m1 = lm(fy_gpa ~ hs_gpa, data=satgpa)
m1

coef(m1)
plot(fy_gpa ~ hs_gpa, data = satgpa)
abline(coef(m1), lwd = 2)

plot(fitted(m1), resid(m1))

## b)
coef(m1)
confint(m1)
summary(m1)$coefficients

## c)
predict(m1, data.frame(hs_gpa = c(3.5)), interval = 'confidence')
predict(m1, data.frame(hs_gpa = 3.5), interval = 'prediction')

## d)
m2 <- lm( fy_gpa ~ hs_gpa + sat_sum, data=satgpa )
summary(m2)
plot( fitted(m2), resid(m2) )

m3 <- lm( fy_gpa ~ hs_gpa + sat_m, data=satgpa )
summary(m3)
plot( fitted(m3), resid(m3) )

m4 <- lm( fy_gpa ~ hs_gpa + sat_v, data=satgpa )
summary(m4)
plot( fitted(m4), resid(m4) )

summary(m1)$adj.r.squared
summary(m2)$adj.r.squared
summary(m3)$adj.r.squared
summary(m4)$adj.r.squared

## task_100


# task_103


# task_104
## yes, because 0 is in the interval


# task_105
data = c(25, 29, 18, 29, 22, 20, 27, 24, 20, 29, 18, 20, 31, 25, 21, 24, 24, 21, 18, 24, 24, 29, 25, 24, 27, 22, 25, 22, 27, 25)
t.test(data, mu = 25, alternative = 'less')$p.value < 0.05 # no!


# task_106
x = c(144, 170, 158, 172, 148, 152, 156)
n = length(x)
probs = rep(1/n, each = n)
chisq.test(x, p = probs)
chisq.test(x, p = probs)$p.value # they are not equal
chisq.test(x, p = probs)$expected


# task_107
tt = read.table("DataPract/tomato2.txt", header = T)
t.test(tt$v1, tt$v2, alternative = 'less')$p.value < 0.05 # we can


# task_108
n1 = 30
x1 = 22

n2 = 30
x2 = 16

x = c(x1, x2)
n = c(n1, n2)

prop.test(x, n, alternative = 'greater') # we can not
prop.test(x, n, alternative = 'greater')$p.value < 0.05


# task_109
n = 73
tt = read.table("DataPract/books.txt", header = T)

result = t.test(tt$price1, tt$price2, paired = T, alternative = 'greater')
result
result$p.value < 0.05 # yes we can

# task_110
x1 = 89
n1 = 500

x1/n1

p2 = 0.17

prop.test(x1, n = n1, p = p2, alternative = 'greater')$p.value < 0.05 # we can not

prop.test( x=1, n=500, p=0.035, alternative="less", correct=F )
