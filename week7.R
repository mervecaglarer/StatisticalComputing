library(MASS)
mydata<- MASS :: birthwt
colnames(birthwt) <- c("birthwt.below.2500", "mother.age", "mother.weight",
                       "race", "mother.smokes", "previous.prem.labor", "hypertension", 
                       "uterine.irr", "physician.visits", "birthwt.grams")
mean(birthwt$birthwt.grams)
boxplot(birthwt$birthwt.grams)

#trimmed mean
mean(birthwt$birthwt.grams, trim = 0.50)
mean(birthwt$birthwt.grams, trim = 0.60) #result doesnt change because default is 0.50

#missing values
is.na(birthwt$birthwt.grams)
table(is.na(birthwt$birthwt.grams))

hist(birthwt$birthwt.grams)
d<-density(birthwt$birthwt.grams)
plot(d)

#4 healthy kids in 5 kids with probability of being healhty is 0.75
dbinom(x=4,size=5,prob=0.75)

#at least 4 healthy kids in 5 kids with probability of being healhty is 0.75
pbinom(q=4,size=5,prob=0.75)

#generate 100 cases for this story
rbinom(n=100,size = 6,prob = 0.75)

#prob=0.5 how many kids are healthy
qbinom(0.5,5,0.75)

#poisson distribution
dpois(12,1.2)
1-dpois(12,1.2)
ppois(1,1.2)
rpois(100,1.2)
qpois(0.8,1.2)
