library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ppcor)
library(car)

## load data
data = read_excel("Data for the Final Project.xlsx")


## initial model
fit1 = lm(Highschool~GDP+Gini+Urbanity+Export+`Unemployment rate`+`Cost of living`+Population+`Union density`,data)
summary(fit1)
plot(fit1)

## corplot
crPlots(fit1)

## partial correlation
n = length(data$Highschool)
regress.high = lm(Highschool~GDP+Gini+Urbanity+Export+`Unemployment rate`+`Cost of living`+Population, data = data)
regress.Union = lm(`Union density`~GDP+Gini+Urbanity+Export+`Unemployment rate`+`Cost of living`+ Population, data = data)
residuals.high = residuals(regress.high)
residuals.Union = residuals(regress.Union)
plot(residuals.high,residuals.Union)
abline(lm(residuals.Union~residuals.high), col="red")
qqplot(qnorm(seq(0,1,length=101),mean(residuals.high), sd(residuals.high)),residuals.high)
partial.correlation = cor(residuals.high, residuals.Union, method = 'spearman')
test.statistic = partial.correlation*sqrt((n-3)/(1-partial.correlation^2))
p.value = 2*pt(abs(test.statistic), n-3, lower.tail = F)
a= lm(residuals.Union~residuals.high)
plot(a)

##transformation model
fit2 = lm(log(Highschool)~log(GDP)+Gini+Urbanity+log(Export)+`Unemployment rate`+`Cost of living`+log(Population)+`Union density`, data = data)
summary(fit2)
plot(fit2)

## pertial correlation of transformation model
regress.high.t = lm(log(Highschool)~log(GDP)+Gini+Urbanity+log(Export)+`Unemployment rate`+`Cost of living`+log(Population), data = data)
regress.Union.t = lm(`Union density`~log(GDP)+Gini+Urbanity+log(Export)+`Unemployment rate`+`Cost of living`+log(Population), data = data)
residuals.high.t = residuals(regress.high.t)
residuals.Union.t = residuals(regress.Union.t)
plot(residuals.high.t,residuals.Union.t)
abline(lm(residuals.Union.t~residuals.high.t), col="red")
qqplot(qnorm(seq(0,1,length=101),mean(residuals.high.t), sd(residuals.high.t)),residuals.high.t)
partial.correlation.t = cor(residuals.high.t, residuals.Union.t, method = 'spearman')
test.statistic.t = partial.correlation.t*sqrt((n-3)/(1-partial.correlation.t^2))
p.value.t = 2*pt(abs(test.statistic.t), n-3, lower.tail = F)
b = lm(residuals.Union.t~residuals.high.t)
plot(b)

## corplot of transformation model
crPlots(fit2)


