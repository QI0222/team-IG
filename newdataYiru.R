library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ppcor)
library(car)
library(tidyverse)
library(lme4)

newdata = read_csv("new data.csv")
newdata <- newdata[,1:19]
data = na.omit(newdata)

data = data%>% 
  mutate( Other = Totalpop-Black_E-White_E-Hispanic_E)
data = data %>%
  mutate(Racial_diversity = 1-White_E^2-Black_E^2-Hispanic_E^2-Other^2)

data = data %>%
  mutate(norm_RD = Racial_diversity *(4/3))




fit1 = lm(Voe~GDP+factor(Year)+Urbanity+Unemployment+Totalpop+Union,data = data)
summary(fit1)
plot(fit1)
crPlots(fit1)


fit2 = lm(log(Voe)~log(GDP)+factor(Year)+Urbanity+log(Unemployment)+log(Totalpop)+Union,data = data)
summary(fit2)
plot(fit2)
crPlots(fit2)

fit3 = lm(log(Voe)~log(GDP)+Urbanity+Unemployment+log(Totalpop)+Union + scale(norm_RD),data = data)
summary(fit3)
plot(fit3)
crPlots(fit3)

