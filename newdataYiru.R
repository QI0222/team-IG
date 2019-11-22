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
  mutate(Black = Black_E/Totalpop)
data = data%>%
mutate(White = White_E/Totalpop)
data = data%>%
mutate(Hispanic = Hispanic_E/Totalpop)

data = data %>%
  mutate(Racial_diversity = 1-White^2-Black^2-Hispanic^2)

data = data %>%
  mutate(norm_RD = Racial_diversity *(3/2))




fit1 = lm(Voe~GDP+factor(Year)+Urbanity+Unemployment+Totalpop+Union+ giniE,data = data)
summary(fit1)
plot(fit1)
crPlots(fit1)


fit2 = lm(log(Voe)~log(GDP)+factor(Year)+Urbanity+log(Unemployment)+log(Totalpop)+Union+log(giniE),data = data)
summary(fit2)
plot(fit2)
crPlots(fit2)

fit3 = lm(log(Voe)~log(GDP)+Urbanity+Unemployment+log(Totalpop)+Union + scale(norm_RD)+log(giniE),data = data)
summary(fit3)
plot(fit3)
crPlots(fit3)

