mydata <- InsectSprays
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
mydata <- ggplot2::diamonds
mydata

mydata [mydata$carat > 0.50 & mydata$color=="E",]
mydata [mydata$carat > 0.50 | mydata$color=="E",]


mydata1 <- subset(mydata , color="E")
mydata1
mydata2 <- transform(mydata, carat= log(carat))
mydata2

