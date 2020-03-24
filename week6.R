mad<-function(x){
  sum(abs(x-mean(x)))/length(x)-1
}

library(MASS)
mydata<- MASS :: birthwt
colnames(birthwt) <- c("birthwt.below.2500", "mother.age", "mother.weight",
                       "race", "mother.smokes", "previous.prem.labor", "hypertension", 
                       "uterine.irr", "physician.visits", "birthwt.grams")

mad(birthwt$birthwt.grams)
summary(birthwt$birthwt.grams)

#changing to order of items (Min 1st Qu Median Mean 3rd Qu Max) to (L mean U median stdev IQR MAD)
summary5<-function(x){
  Lower<-mean(x) - 3*(sd(x)/length(x)^(1/2))
  Upper<-mean(x) + 3*(sd(x)/length(x)^(1/2))
  
  c(L=Lower,mean=mean(x),U=Upper, median=median(x), stdev=sd(x), IQR=IQR(x), MAD=mad(x), min=min(x),max=max(x))
}

summary5(birthwt$mother.age)

#adding new column 
givecategory <- function(x){
  if(x<=1500){
    category <- 1
  }else if(x<=2500){
    category <- 2
  }else if(x<=3500){
    category<- 3
  }else if(x<=4500){
    category<- 4
  }else{
    category<- 5
  }
  category
}

birthwt$category<-sapply(birthwt$birthwt.grams, FUN = givecategory)

boxplot(birthwt$birthwt.grams~birthwt$race,
        col=c("blue","yellow","red"),
        main="Boxplot of Weights",
        ylab="Weight",
        xlab="Race")
boxplot.stats

plot(birthwt$birthwt.grams,birthwt$mother.age)
d<- density(birthwt$birthwt.grams)
plot(d)

hist(birthwt$birthwt.grams,breaks = 5)
?hist

library(ggplot2)
ggplot(data = birthwt,aes(x=birthwt.grams,y=mother.age,col=race))+
  geom_point()+
  ylab("Weight")+
  xlab("Age")+
  facet_wrap(~mother.smokes)+
  theme_light()

g<-ggplot(data=birthwt,aes(x=1:length(mother.age),y=mother.age))+
  geom_boxplot()+
  facet_wrap(~race)
g

fancy.hist<-function(x,data){
  c=sample(colours(),1)
  f=sample(colours(),1)
  ggplot(data = data,aes(x=x))+
    geom_histogram(col=f,fill=c)
}
fancy.hist(birthwt$birthwt.grams,birthwt)

