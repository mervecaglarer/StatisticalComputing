iris
table(iris$Species)
mean(iris$Sepal.Length)

#Ho:mu<=6
#Ha:mu>6

#Do I know the standard deviation?
#No
#t-test

t.test(iris$Sepal.Length, 
       mu=6,
       alternative = "greater",
       conf.level = 0.95)

#p-value=0.98>0.05 Ho not reject
#95%CI 5.73 +INF

#16 students are randomly selected and we measured their grades for a particular 
#exam. suppose the teacher applies a method and the following values are before 
#method values.and the teacher claims that the average grade of this class is 
#greater than 50.
before.treatment<-c(25,41,41,54,29,50,54,46,54,33,33,54,37,12,29,41)

#Normality check
hist(before.treatment)
boxplot(before.treatment)
shapiro.test(before.treatment)
#p-value>0.05 Ho for normality cannot be rejected meaning the dist is normal.

#Ho: mu<=50
#Ha: mu>50

t.test(before.treatment, 
       mu=50,
       alternative = "greater",
       conf.level = 0.95)

#pvalue>0.05 Ho cannot be rejected. We dont have sufficient evidence to support Ha. 

#In this experiment, think that the teacher lets the students watch the record.
before.video<-c(25,41,41,54,29,50,54,46,54,33,33,54,37,12,29,41)
after.video<-c(41,66,92,71,71,54,88,54,70,50,58,79,88,46,67,46)

#If the sampling unit is the same, the measurement are paired or dependent. 
#Paired cases or dependent samples

diff<-after.video-before.video 
mean(diff)

#I want to test the efficiency of video recording. The teacher want to learn if videos make a difference.
#Ho: mu.diff=0
#Ha: mu.diff=!0

#Normality
shapiro.test(diff)
#p-value=0.38>0.05 Normal

#I. way
t.test(diff,
       mu=0,
       alternative = "two.sided",
       conf.level = 0.95)

qt(0.025, 15)

#p-value=0.000<0.05 Ho reject
#95%CI [17.5, 33.5] doesnt include 0.

#II.way

#Ho: mu.after=mu.before
#Ha: mu.after=!mu.before 

#Normality
shapiro.test(after.video)
shapiro.test(before.video)

t.test(after.video, before.video,
       alternative = "two.sided",
       conf.level = 0.95,
       paired = TRUE)

#The teacher claims the mean difference in the grades are >10 points.

#Ho: mu.after-mu.before<=10
#Ha: mu.after-mu.before>10

t.test(after.video, before.video,
       alternative = "greater",
       mu=10,
       conf.level = 0.95,
       paired = TRUE)

#p-value<0.05 Ho reject
#95%CI =[18.9, +INF)

#TWO INDEPENDENT SAMPLES CASE
GradesA<-c(55,41,51,54,29,50,64,46,54,33,33,34,37,12,29,41)
GradesB<-c(41,66,92,91,71,54,88,54,90,50,58,79,98,56,67,46)

#The teacher wants to look if the mean grades are equal or not for classA and classB


#1. Normality check
shapiro.test(GradesA) #Normal
shapiro.test(GradesB) #Normal

#2. Check dependency
#We have two independent classes so the sampling units (students) are totally 
#different. So, we will conduct an independent t-test.

#3. Variances of the two independent samples are the same?
#Variance homogenity

#Ho: sigmaA=sigmaB
#Ha: sigmaA=!sigmaB
var.test(GradesA, GradesB)
#p-value>0.05 not reject null, variances are homogenious

#4. test the hypothesis
#Ho: muA=muB
#Ha: muA=!muB

t.test(GradesA, GradesB,
       paired=FALSE,
       conf.level = 0.95,
       alternative = "two.sided",
       var.equal = TRUE)

#p-value<0.05 reject null
#95%CI= [-38.99, -15.75]