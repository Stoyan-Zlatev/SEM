# Code for simulating the Birthday line problem
days = 365
max_people = 366

task2.experiment = function(){
  people = sample(days, max_people, replace = T)
  anyDuplicated(people)
}

task2.prob = function(Nrep){
  result = replicate(Nrep, task2.experiment())
  prop.table(table(result))
}

set.seed(666)

table = task2.prob(10^7)
table[which.max(table)]
table[10:30]
barplot(table[10:30], main="Results of probability of winning by simulating the game")


                