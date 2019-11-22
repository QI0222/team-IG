#New data
pacman::p_load(readxl,dplyr,tidyverse,ggplot2,plotly,ggpubr,MASS,fitdistrplus,mediation,geepack,conf,car,knitr,gclus,scatterplot3d, ppcor,lme4,mediation,quantreg)
newdata <- read.csv("new data.csv")
summary(newdata)

#Try to analysis the data in the single year (2017), which is the same year as the old dateset.
#Create a new dataset that contains only the data in the year of 2017 (data1)
data1 <- filter(newdata,Year=="2017")

#data transformation: generate a new variable called GV to measure the level of racial diversity
#Create a new dataset(data2) that includes generalized variety
#Have to change the raw number of White, Black, Hispanic to proportion of the whole population.
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

#Multilevel linear regression:fixed effect
#Consider the yearly data as varying intercept and then fit the model
#Create a new dataset(data3) that excludes year between 2005 and 2008
data3 <- filter(newdata,Year>2008)
fit4 <- lmer(log(Voe)~Union+Urbanity+log(Unemployment)+log(GDP)+log(Totalpop)+(1|Year),data=data3)
summary(fit4)

#Create a new dataset(data3) that includes normalized racial diversity
data4 <- data3 %>% 
  mutate(Racial_diversity=1-White_E^2-Black_E^2-Hispanic_E^2) %>%
  mutate(norm_Racial_diversity = Racial_diversity*(3/2))
summary(data4)

#Mediation Analysis
model.m <- lmer(norm_Racial_diversity~Union+Urbanity+log(Unemployment)+log(GDP)+log(Totalpop)+(1|Year),data=data4)
model.y <- lmer(log(Voe)~Union+norm_Racial_diversity+Urbanity+log(Unemployment)+log(GDP)+log(Totalpop)+(1|Year),data=data4)
out.1 <- mediate(model.m,model.y,sims=1000,treat = "Union",mediator = "norm_Racial_diversity")
summary(out.1)
plot(out.1)
#In this case, we find the racial diversity does have a mediation effect.

