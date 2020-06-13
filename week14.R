#Coklu regresyon modeli icin gerekli paketler
library(GGally)
library(car)
library(mlbench)

#Gorsellestirme ve veri kesfi
data("BostonHousing")
hist(BostonHousing$medv)
y<-BostonHousing[,14]
x<-BostonHousing[,-14]
cor.matrix<-cor(BostonHousing[,c(-4,-14)]) #Korelasyon matrisi
plot(BostonHousing[,c(-4,-14)])
ggpairs(BostonHousing[,-14]) #Scatter plot matrisi
ful.model<-lm(medv~., data=BostonHousing)
summary(ful.model)
vif(ful.model)

#Modelleme
model<-lm(medv~crim+zn+indus+chas+rm+age+dis+ptratio+b+lstat, data=BostonHousing)

#Modeli yorumlama
summary(model)
indirgenmis.model<-step(model)
summary(indirgenmis.model)

#Varsayimlarin kontrolu
plot(indirgenmis.model)

#Lojistik Regresyon Modeli
data("PimaIndiansDiabetes")
plot(PimaIndiansDiabetes$age,
     as.numeric(PimaIndiansDiabetes$diabetes))
model1<-lm(as.numeric(diabetes)~age, data=PimaIndiansDiabetes)
summary(model1)
plot(model1)

ful.model<-lm(as.numeric(diabetes)~., data=PimaIndiansDiabetes )
summary(ful.model)
plot(ful.model)

ggpairs(PimaIndiansDiabetes[,-9]) #Scatter plot matrisi
glm.model<-glm(diabetes~., data=PimaIndiansDiabetes, family = binomial(link = "logit"))
summary(glm.model)
glm.model<-glm(diabetes~., data=PimaIndiansDiabetes[,-c(4,5,8)], family = binomial(link = "logit"))
summary(glm.model)