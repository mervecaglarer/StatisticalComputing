library(MASS)
mydata<-MASS::birthwt
mean(mydata$bwt)
sd(mydata$bwt)
#Example1: From this sample I want to test that the mean weight for the babies
#is 2000gr on 95% confidence
#Ho:mu = 2000
#Ha:mu not= 2000

#standard dev is not known, we need to use a one sample t-test
t.test(mydata$bwt, 
       mu=2000, 
       conf.level = 0.95, 
       alternative = c("two.sided"))
#1 way
#t(o)=17.8 observed value
#t(t)=approximately around 2 or exactly t(t)= As t(o)>t(t) reject null

qt(0.025, (189-1))

#2 way
#pvalue=0.000000<0.05 reject null

#3 way
#P(2839<mu<3049)=0.95
#mu=2000 is not included in this interval so you can reject the null.

#Example2: Consider an claim saying that the babies mean weight is greater than 2500gr
#Ho: mu<=2500
#Ha: mu>2500

t.test(mydata$bwt, 
       mu=2500, 
       conf.level = 0.95, 
       alternative = c("greater"))

#t(o)=8>t(t) reject null
#pvalue<alpha=0.05 reject null
##P(2856<mu<+inf)=0.95
#mu=2500 is not included in this interval so you can reject the null.