#New data
pacman::p_load(readxl,dplyr,tidyverse,ggplot2,plotly,ggpubr,MASS,fitdistrplus,mediation,geepack,conf,car,knitr,gclus,scatterplot3d, ppcor)
newdata <- read.csv("new data.csv")
summary(newdata)
data1 <- filter(newdata,Year=="2017")
#data transformation: generate a new variable called GV to measure the level of racial diversity
data2 <- data1 %>% 
  mutate(Racial_diversity=1-White_E^2-Black_E^2-Hispanic_E^2) %>%
  mutate(norm_Racial_diversity = Racial_diversity*(3/2))
summary(data2)
#Check the Cr plots for test model
fittest <- lm(Voe~Union+Urbanity+Unemployment+GDP+Totalpop,data = data2)
summary(fittest)
plot(fittest)
crPlots(fittest)

#transformation
#fit3 seems to be the best with the least outliners and more valid assumptions
#Still, in the residuals versus leverage plot, there is a outlier that could impact the coefficient.
#Need to check out which state it is
fit1 <- lm(log(Voe)~Union+Urbanity+log(Unemployment)+log(GDP)+log(Totalpop),data=data2)
summary(fit1)
plot(fit1)

fit2 <- lm(log(Voe)~Union+Urbanity+Unemployment+GDP+Totalpop,data = data2)
crPlots(fit2)
summary(fit2)
plot(fit2)

fit3 <- lm(log(Voe)~Union+Urbanity+log(Unemployment)+log(GDP)+log(Totalpop),data = data2)
crPlots(fit3)
summary(fit3)
plot(fit3)


