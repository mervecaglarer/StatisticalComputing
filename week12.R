#Experiment: searching for drug effect for migrene
#Four different pain killers
#Each group is independent and randomly selected
#Each group has 10 individuals
#We ask each patient to use a certain brand 
#Then evaluate their pain in a scale of 0-10.
A <- c(4.4, 5.1, 4.3, 3.3, 2.1, 4.7, 3.3, 4.5, 4.2, 4.2)
B <- c(6.9, 8.1, 4.2, 5.5, 4.3, 6.6, 5.4, 8.2, 6.3, 6.3)
C <- c(6.7, 7.1, 6.4, 6.6, 7.2, 5.4, 6.0, 5.0, 5.2, 5.2)
D <- c(6.6, 4.1, 4.4, 5.6, 8.2, 6.4, 6.0, 5.0, 5.2, 5.2)

#1) visualisation
pain <- c(A,B,C,D)
drug <- as.factor(c(rep("A",10), rep("B",10), rep("C",10), rep("D",10)))
migrene = data.frame(pain,drug)
boxplot(pain~drug, data=migrene)

#2) Assumptions
shapiro.test(A) #Normal
shapiro.test(B) #Normal
shapiro.test(C) #Normal
shapiro.test(D) #Normal

#Variance homogenety
#H0:var1=var2=var3=var4
#Ha:At least one of them is different
bartlett.test(pain~drug, data=migrene)
#p-value=0.38>0.05 Not reject the null. Assume variances are equal.

#3) Analysis of variance
#H0:mu1=mu2=mu3=mu4
#Ha:At least one of them is different
model<-aov(pain~drug, data=migrene)
model$coefficients
summary(model)
qf(1-0.05, 3, 36)

#4) Find out who is different? Posthoc test
TukeyHSD(model)

#5) Distribution of the error term. Linear error terms 
#should include some proporties if the assumptions are true
plot(model)

#TWO-WAY ANOVA
resting<- as.factor( c(rep("Yes",5), rep("No",5),
                       rep("Yes",5), rep("No",5),
                       rep("Yes",5), rep("No",5),
                       rep("Yes",5), rep("No",5)))
migrene = data.frame(pain,drug,resting)
boxplot(pain~drug+resting, data=migrene)

model2way<-aov(pain~drug+resting, data=migrene)
summary(model2way)

TukeyHSD(model2way)
