library(MASS)
data(survey)
survey
View(survey)

table(survey$Exer)

attach(survey)

# Task 43

table(Exer)
sort(table(Exer), decreasing = T)
100*table(Exer)/length(Exer) # in %


barplot(table(Exer))
barplot(sort(table(Exer), decreasing = T))
barplot(100*table(Exer)/length(Exer) )

pie(table(Exer))
pie(table(Exer), col=c("red", "yellow", "blue"))
pie(sort(table(Exer), decreasing = T))
pie(100*table(Exer)/length(Exer) )


# Task 44

table(Pulse)
table(Pulse, useNA="ifany")

pulse.grp <- cut(Pulse, breaks=seq(30,110,10))
pulse.grp
table(pulse.grp)
barplot(table(pulse.grp))

hist(Pulse)
hist(Pulse, breaks=seq(30,110,10))

stripchart(Pulse, method="stack", pch=20)
stripchart(Pulse, method="stack", pch=18)
stripchart(Pulse, method="stack", pch="*")


# Task 45
table(Age)

age.grp <- cut(Age, breaks=seq(15,75,10))
table(age.grp)

barplot(table(age.grp))
hist(Age)

stripchart(Age,method="stack", pch=20)

# Task 46

v1 <- rep(4,30)
v2 <- rep(c(4.5,3.5),15)
v3 <- rep(c(3,5),15)
v4 <- rep(c(2:6),15)
v5 <- rep(c(2,6),15)

par(mfrow=c(2,3)) # 2 rows and 3 columns of graphs
stripchart(v1,pch=20,method="stack", xlim=c(2,6),ylim=c(0,10))
stripchart(v2,pch=20,method="stack", xlim=c(2,6),ylim=c(0,10))
stripchart(v3,pch=20,method="stack", xlim=c(2,6),ylim=c(0,10))
stripchart(v4,pch=20,method="stack", xlim=c(2,6),ylim=c(0,10))
stripchart(v5,pch=20,method="stack", xlim=c(2,6),ylim=c(0,10))

median(v1)
median(v2)
median(v3)
median(v4)
median(v5)

mean(v1)
mean(v2)
mean(v3)
mean(v4)
mean(v5)

sd(v1)
sd(v2)
sd(v3)
sd(v4)
sd(v5)

# Task 47

par(mfrow=c(1,1))

getwd()
load("cereals.RData")

attach(cereals)
summary(carbo)

sd(carbo, na.rm=T)

boxplot(carbo, horizontal=T)
hist(carbo)

summary(sodium)
sd(sodium,na.rm=T)
boxplot(sodium, horizontal=T)
hist(sodium)

summary(potass)
sd(potass,na.rm=T)
boxplot(potass, horizontal=T)
hist(potass)


# Task 48

boxplot(Pulse ~ W.Hnd)

mean(Pulse[W.Hnd == "Left"], na.rm=T)
mean(Pulse[W.Hnd == "Right"], na.rm=T)

median(Pulse[W.Hnd == "Left"], na.rm=T)
median(Pulse[W.Hnd == "Right"], na.rm=T)


# Task 49

my.summary <- function(x) {
  res <- c(median(x,na.rm=T),mean(x,na.rm=T),sd(x,na.rm=T))
  names(res) <- c("Median", "Mean", "StDev")
  res
}

# a
my.summary(Pulse)

# b
my.summary(Pulse[Sex=="Female"])

# c
my.summary(Pulse[Age<=25])

# d
my.summary(Pulse[Exer=="Freq"])

# e
my.summary(Pulse[Exer=="Freq" & Smoke=="Never"])


# Task 50

boxplot(Pulse ~ Exer)

mean(Pulse[Exer=="Freq"], na.rm=T)
mean(Pulse[Exer=="Some"], na.rm=T)
mean(Pulse[Exer=="None"], na.rm=T)

median(Pulse[Exer=="Freq"], na.rm=T)
median(Pulse[Exer=="Some"], na.rm=T)
median(Pulse[Exer=="None"], na.rm=T)
