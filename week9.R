library(MASS)
library(plyr)
library(ggplot2)

# Rename the columns to have more descriptive names
colnames(birthwt) <- c("birthwt.below.2500", "mother.age", "mother.weight",
                       "race", "mother.smokes", "previous.prem.labor", "hypertension", "uterine.irr",
                       "physician.visits", "birthwt.grams")
# Transform variables to factors with descriptive levels
birthwt <- transform(birthwt,
                     race = as.factor(mapvalues(race, c(1, 2, 3),
                                                c("white","black", "other"))),
                     mother.smokes = as.factor(mapvalues(mother.smokes,
                                                         c(0,1), c("no", "yes"))),
                     hypertension = as.factor(mapvalues(hypertension,
                                                        c(0,1), c("no", "yes"))),
                     uterine.irr = as.factor(mapvalues(uterine.irr,
                                                       c(0,1), c("no", "yes")))
)

#Our sample has size of 189
#mean of birth weight grams
mean(birthwt$birthwt.grams)
#standard deviation of birth weight grams
sd(birthwt$birthwt.grams)
#standard error of birth weight grams
se<-sd(birthwt$birthwt.grams)/sqrt(length(birthwt$birthwt.grams))

#variance or sigma^2 is known
conf.interval = function(data, standard.dev, conf.level = 0.95) {
  z = qnorm(1-(1 - conf.level)/2)
  xbar = mean(data)
  se = standard.dev/sqrt(length(data))
  c(lower.limit=xbar - z * se, upper.limit=xbar + z * se)
}
#confidence interval of birthwt grams
#provide variance
birthwt.sd<-800
conf.interval(birthwt$birthwt.grams, birthwt.sd, conf.level = 0.95)

#confidence interval of birthwt grams sd is not known
conf.interval.sd.notknown = function(data, conf.level = 0.95) {
  t = qt(1-(1 - conf.level)/2, df=(length(data)-1))
  xbar = mean(data)
  se = sd(data)/sqrt(length(data))
  c(lower.limit=xbar - t * se, upper.limit=xbar + t * se)
}
conf.interval.sd.notknown(birthwt$birthwt.grams, conf.level = 0.95)

#Base R
#st dev is not known case
t.test(birthwt$birthwt.grams)$conf.int
