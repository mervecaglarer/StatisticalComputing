install.packages("MASS")
library(MASS)
birthwt

#Min can be calculated but it is not logical!

mydata<- data(birthwt)
mydata<- MASS :: birthwt

str(mydata)
#change the column names
colnames(birthwt) <- c("birthwt.below.2500", "mother.age", "mother.weight",
                       "race", "mother.smokes", "previous.prem.labor", "hypertension", 
                       "uterine.irr", "physician.visits", "birthwt.grams")
library(plyr)
birthwt <- transform(birthwt,
                     race = as.factor(mapvalues(race, c(1, 2, 3),
                                                c("white","black", "other"))),
                     mother.smokes = as.factor(mapvalues(mother.smokes,
                                                c(0,1), c("no", "yes"))),
                     hypertension = as.factor(mapvalues(hypertension,
                                               c(0,1), c("no", "yes"))),
                     uterine.irr = as.factor(mapvalues(uterine.irr,
                                              c(0,1), c("no", "yes"))),
                     birthwt.below.2500 = as.factor(mapvalues(birthwt.below.2500,
                                                     c(0,1), c("no", "yes"))))
str(birthwt)
mean(birthwt$mother.age)
sd(birthwt$mother.age)

boxplot(birthwt$mother.age)
boxplot(birthwt$birthwt.grams ~ birthwt$mother.smokes)

min(birthwt[birthwt$mother.smokes=="yes",10])
median(birthwt$birthwt.grams)
IQR(birthwt$birthwt.grams)
sd(birthwt$birthwt.grams)
summary(birthwt$birthwt.grams)
fivenum(birthwt$birthwt.grams)

#tapply function
with(birthwt, tapply(birthwt.grams, INDEX = list(race, mother.smokes), FUN = mean))
#aggregate function
with(birthwt, aggregate(birthwt.grams, by = list(race, mother.smokes), FUN = mean))
with(birthwt, aggregate(birthwt.grams, by = list(race, hypertension), FUN = mean))

with(birthwt, table(birthwt.below.2500, mother.smokes,race))

#plots
plot(birthwt$birthwt.grams)
plot(birthwt$birthwt.grams, birthwt$mother.age)
par(mfrow=c(2,1))
#if I want to go one by one plot , change to c(1,1)
par(mfrow=c(1,1))
plot(birthwt$mother.weight,birthwt$birthwt.grams)
plot(birthwt$birthwt.grams,birthwt$mother.age)

#histograms
hist(birthwt$birthwt.grams,
     xlab = "birth weight grams",
     main = "",
     col = "navyblue")
hist(birthwt$birthwt.grams,
     xlab = "birth weight grams",
     main = "",
     col = "navyblue",
     density = TRUE,
     nclass = 10)

d<-density(birthwt$birthwt.grams)
plot(d)
plot(birthwt$mother.smokes)
