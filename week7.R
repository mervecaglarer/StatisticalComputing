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

#binomial probabilities
# Suppose there are twelve multiple choice questions in an English class quiz. 
# Each question has five possible answers, and only one of them is correct. 
# Find the probability of having four or less correct answers if a student attempts 
# to answer every question at random.

# Since only one out of five possible answers is correct, the probability 
# of answering a question correctly by random is 1/5=0.2. We can find the 
# probability of having exactly 4 correct answers by random attempts as follows.
#P(X=4)=?
dbinom(4, size=12, prob=0.2) 

# To find the probability of having four or less correct answers by random attempts, 
# we apply the function dbinom with x = 0,…,4.
#P(X>=0)=?
dbinom(0, size=12, prob=0.2) + 
  + dbinom(1, size=12, prob=0.2) + 
  + dbinom(2, size=12, prob=0.2) + 
  + dbinom(3, size=12, prob=0.2) + 
  + dbinom(4, size=12, prob=0.2) 

#Alternatively, we can use the cumulative probability function for 
#binomial distribution pbinom.

pbinom(4, size=12, prob=0.2) 

#Generate 100 binomial observations
rbinom(100,size=12, prob=0.2)

#Poisson distribution
# If there are twelve cars crossing a bridge per minute on average, 
# find the probability of having seventeen or more cars crossing the 
# bridge in a particular minute.

#The probability of having exactly sixteen cars crossing the bridge in 
# a particular minute is given by the function ppois.
#P(X=16)=?
dpois(16, 12)

# The probability of having sixteen or less cars crossing the bridge in 
# a particular minute is given by the function ppois.
# If there are twelve cars crossing a bridge per minute on average, 
# find the probability of having seventeen or more cars crossing the 
# bridge in a particular minute.
#P(X>16)=?
ppois(16, 12)

# Hence the probability of having seventeen or more cars crossing the bridge 
# in a minute is in the upper tail of the probability density function
#P(X>16)=1-P(X<=16)
1-ppois(16, lambda=12)

#generate 100 random numbers from this distribution
rpois(100, 12)