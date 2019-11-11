## package
library(knitr)
library(tidyverse)
library(kableExtra)
library(shiny)
library(car)
library(gclus)
library(scatterplot3d)
attach(new)
library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ppcor)



## data process
new<-select(data,1,3,5:11,22,23,27:29)
new$State<-as.numeric(new$State)
new$GDP<-as.numeric(new$GDP)
new$Gini<-as.numeric(new$Gini)
new$Urbanity<-as.numeric(new$Urbanity)
new$Population<-as.numeric(new$Population)
new$Export<-as.numeric(new$Export)
new$Unemployment.rate<-as.numeric(new$Unemployment.rate)
new$Highschool<-as.numeric(new$Highschool)
new$Union.density<-as.numeric(new$Union.density)

new$ratio<-as.numeric(new$Highschool)/as.numeric(new$Population)

## rough relationship without control
da <- read.csv("Data for the Final Project.csv")
data <- da[1:51,1:31]


ggplot(data,aes(x=White, y = Union.density)) + 
  geom_point(color='#3399CC',size=4)+
  geom_smooth(method = lm,color='#3366FF')

ggplot(data,aes(x=Black, y = Union.density)) + 
  geom_point(color='#CC6666',size=4)+
  geom_smooth(method = lm,color='#663300')

ggplot(data,aes(x=Hispanic, y = Union.density)) + 
  geom_point(color='#00CC66',size=4)+
  geom_smooth(method = lm,color='#006666')

## other plots that see the relationships
symbols(Union.density,Highschool,circles = new$White/pi,inches=0.30, fg="white", bg="lightblue") 


car::spm(~Union.density+Highschool, data = new,  
         smooth=list(lty.smooth=2,lwd.smooth = 3, col.smooth="red", spread = T, lty.spread=3, lwd.spread=2),  
         diagonal = list(method="density"), regLine=list(method =lm,lty=1,lwd=2,col="blue"), 
         cex = 1, cex.labels = 1.5, cex.axis = 1.5, 
         pch = c(16,16,16), col = c("red", "green3", "blue"), row1attop = T ) 

s3d<-scatterplot3d(Union.density,White,Highschool, pch=16, highlight.3d=TRUE,type="h") 
f <- lm(Highschool~Union.density+White) 
s3d$plane3d(f,lty.box = "solid")

## corelation test
cor(news,method = "spearman")


## cr-plot
colnames(data)[2] <- "uniondensity"
fit1 <- lm(Highschool ~ uniondensity+ GDP+Urbanity+Population+Export+`Cost of living`+`Unemployment rate`+Gini,data)

fit2 <- lm(Highschool ~ uniondensity+ GDP+Urbanity+log(Population)+Export+`Cost of living`+`Unemployment rate`+Gini,data)

fit3 <- lm(Highschool ~ uniondensity+ GDP+Urbanity+log(scale(Population^1/2))+scale(Export^1/2)+`Cost of living`+`Unemployment rate`+Gini,data)

crPlots(fit1)
crPlots(fit2)
crPlots(fit3)














