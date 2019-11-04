##Political Science
pacman::p_load(readxl,dplyr,tidyverse,ggplot2,plotly,ggpubr,MASS,fitdistrplus,mediation,geepack)
data1<-read_excel("Data for the Final Project.xlsx")
data1<-as_tibble(data1)
#data transformation: generate a new variable called GV to measure the level of racial diversity
mydata<-data1 %>% mutate(GV=1-White^2-Black^2-Hispanic^2)

cor.test(mydata$`Union density`,mydata$Highschool)
#cor.test(mydata$`Union density`,mydata$Highschool,method = c('pearson','kendall','spearman'))
plot1<-ggplot(mydata)+geom_point(aes(x=mydata$Highschool,y=mydata$`Union density`))+
  geom_smooth(aes(x=mydata$Highschool,y=mydata$`Union density`))
plot2<-plot_ly(data=mydata,x=~Highschool,y=~`Union density`, name = `relationship`,
               type='scatter',mode='lines') %>% 
  add_trace(y=~`Union density`,x=~Highschool,mode='lines + markers')
               #line=list(color='rgb(205,12,24)',width=4)
plot3<-ggplot(cleandata, aes(x=cleandata$Highschool,y=cleandata$`Union density`))+geom_point()+geom_smooth()
plot2
plot1
cleandata<-mydata %>% filter(Highschool<=400000)
plot3<-ggplot(cleandata, aes(x=cleandata$Highschool,y=cleandata$`Union density`))+geom_point()+geom_smooth()
plot3

##test if vocational training and unionization have negative relationship 
#ggscatter(mydata,x="mydata$Highchool",y="mydata$`Union density`",
         # conf.int = TRUE,
         # cor.coef = TRUE, cor.method = "pearson",
         # xlab = "Vocational Traning", ylab = "Unionization")
#data visualization
plot1<-ggplot(mydata)+geom_point(aes(x=mydata$Highschool,y=mydata$`Union density`))+
  geom_smooth(aes(x=mydata$Highschool,y=mydata$`Union density`))
plot1
cleandata<-mydata %>% filter(Highschool<=250000)
plot3<-ggplot(cleandata, aes(x=cleandata$Highschool,y=cleandata$`Union density`))+geom_point()+geom_smooth()
plot3

#preleminary test to check the test assumptions (pearson, kendall ,spearman)
#- Is the covariation liner? No. Do we have to deal with nonlinear association between the two variables?
#- Are the data from each of the 2 variables follow a normal distribution?
shapiro.test(mydata$`Union density`)  #is not a normal distribution
ggpubr::ggqqplot(mydata$`Union density`)
ggpubr::ggqqplot(mydata$Highscool)
shapiro.test(mydata$Highschool) #not a normal distribution
#cor.test(mydata$`Union density`,mydata$Highschool)

#Since the two variables are not normal distribuiton, we use boostrap to generate a similar normal distribution and then fit the pearson correlation test 
##Pearson correlation test
union_sample<-sample(mydata$`Union density`,replace=TRUE, size=10000)
df1<-fitdist(union_sample,"norm")
highschool_sample<-sample(mydata$Highschool,replace=TRUE, size=10000)
df2<-fitdist(highschool_sample,"norm")
norm1<-rnorm(100,9.874830,5.049525)
norm2<-rnorm(100,160106.5,224721.2)
cor.test(norm1,norm2)
#build a linear regression model to see if there are negative relationship
mod1<-lm(log(Highschool)~`Union density`+log(GDP)+Gini+Urbanity+log(Population)+log(Export)+`Unemployment rate`+`Cost of living`,data = mydata)

summary(mod1)
plot(mod1,which = 1)
#try mixed effect model
#glm_hs<-geeglm(Highschool~`Union density`,waves=`Union density`,id = `State`,
                 #family=gaussian,data=mydata,corstr="unstructured")
hist(mydata$GDP)
ggplot(mydata)+aes(y= log(Highschool))+
  #geom_histogram(bins=30)+
  geom_point(aes(x = `Union density`))

#try data transformation and use pearson test and Fisher z' method
shapiro.test(mydata$`Union density`)
hist(mydata$`Union density`)
hist(log(mydata$`Union density`))
shapiro.test(log(mydata$`Union density`))
##If they do have a negative correlation, the next step is to do mediation analysis.






