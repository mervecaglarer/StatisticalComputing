x <- 10 
x <- 1:10
typeof(x)
length(x)
sqrt(x)
x <- log(x)
x
?log

as.numeric(x)
is.character(x)
is.na(x)
?is.na

letters
month.name
pi
cos(pi)
x[1]
x[10] <- 10
x[10]

faithful
faithful[272,1]
faithful[272,1:2]
faithful[270,]
faithful[,1]
#all rows that have waiting over 70 (TRUE/FALSE)
faithful[2]>70 #faithful$waiting>70
#show all rows that have waiting over 70 
faithful[faithful$waiting>70,]
#show all rows eruptions over 3 and waiting over 70
faithful[faithful$eruptions>4 & faithful$waiting>70,]
#show all rows eruptions over 3 or waiting over 70
faithful[faithful$eruptions>3 | faithful$waiting>70,]

mydata<-faithful[faithful$eruptions>3 & faithful$waiting>70,]
dim(mydata)
head(mydata)
tail(mydata)
