#Write your own function for correlation analysis

x <- c(43,21,25,42,57,59)
y <- c(99,65,79,75,87,81)
plot(x, y, main="Scatterplot Example",
     xlab="x", ylab="y")
CorrelationAnalyzer <- function(x,y) {
  x.y <- x*y
  x.x <- x*x
  y.y <- y*y
  PearsonCor<- ((length(x) * sum(x.y))- (sum(x) * sum(y))) / 
    (sqrt((length(x) * sum(x.x) - sum(x)^2 ) * (length(x) * sum(y.y) - sum(y)^2)))
  PearsonCor
}  
CorrelationAnalyzer(x,y)

#Write your own function for replacing missing values with the mean

a <- c(43,21,25,42,NA,59)

MissingValueReplacer <- function(a) {
  na_index <- which(is.na(a))        
  mean_a <- mean(a, na.rm=T)
  a[na_index] <- mean_a
  return(a)
}
MissingValueReplacer(a)
