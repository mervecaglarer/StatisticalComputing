#week5.pdf examples
#P(X<31)
pnorm(31,mean = 30,sd=2)
#P(X>31)
1-pnorm(31,mean = 30,sd=2)
#P(X<32)
pnorm(32,mean = 30,sd=2)
#P(26<X<29)
pnorm(29,mean = 30,sd=2) - pnorm(26,mean = 30,sd=2)

#Inverse probability
#what is the largest fish in the lowest %10 of fish?
#P(X<a)=0.10
#a=?
qnorm(0.10,mean = 30,sd=2)

#what is the smallest fish in the upper %10 of fish?
qnorm(0.90,mean = 30,sd=2)

#create 100 fish from this population randomly
data<-rnorm(n=100,mean = 30,sd=2)
hist(data)

#week4.pdf 
#scaling
#z=(x-center)/scale
z<-scale(data,center = 30,scale = 2)
z
boxplot(z)

#normality check
hist(data)
d<-density(data)
plot(d)
qqnorm(data)
qqline(data)

data2<-rexp(100,32)
qqnorm(data2)
qqline(data2)
hist(data2)
