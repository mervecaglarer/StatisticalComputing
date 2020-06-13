#1a Ho: mu1=mu2
#Ha: mu1=!mu2

#1b Ho: mu1<=mu2
#Ha: mu1>mu2

#1c
week1<-c(8,10,9,11,8,7,9,10,9,9)
week2<-c(5,7,5,6,7,5,4,6,5,6)
shapiro.test(week1)
shapiro.test(week2)

#1d
t.test(week1,week2, alternative = "greater", paired = T)

#1e
t.test(week1,week2, alternative = "greater", paired = T, conf.level = 0.99)

#1f
t.1<-t.test(week1)
t.2<-t.test(week2)
t.1$conf.int
t.2$conf.int

#2a
#Ho: mu1=mu2
#Ha: mu1=!mu2

#2b
#Ho: mu1<=mu2
#Ha: mu1>mu2

#1c
group1<-c(8,10,9,11,8,7,9,10,9,9)
group2<-c(5,7,5,6,7,5,4,6,5,6)
shapiro.test(group1)
shapiro.test(group2)

#1d
#H0:sigma1=sigma2
#Ha:sigma1=!sigma2
var.test(group1, group2)
#p-value>0.05 variances are same

t.test(group1, group2, alternative = "greater", paired=F, var.equal = T)
#1e
t.3<-t.test(group1, group2, alternative = "greater", paired=F, var.equal = T, conf.level = 0.99)

#1f
t.3$conf.int

#1h
#H0:mu1<=10
#Ha:mu1>10
t.test(group1, mu=10, alternative = "greater")
#p-value>0.05 not reject the null
qt(1-0.05, length(group1)-1)

#3a
group3<-c(6,7,5,5,7,5,3,6,6,6)
consumption<-c(group1, group2, group3)
groups<-c(rep(1,10), rep(2,10), rep(3,10))
coffee.data<-data.frame(consumption, groups)

shapiro.test(group3)

out<-aov(consumption~as.factor(groups), data=coffee.data)
summary(out)

#3b
TukeyHSD(out)