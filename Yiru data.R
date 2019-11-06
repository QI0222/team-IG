library(readxl)
library(dplyr)
library(corrplot)
library(ggplot2)
library(ppcor)

data = read_excel("Data for the Final Project.xlsx")
df= mutate(data, Ratio = Highschool/Population)
ggplot(df, aes(Black, Highschool)) + geom_point() + geom_smooth()
ggplot(df, aes(Black, Ratio)) + geom_point() + geom_smooth()
ggplot(df, aes(Black, `Perkins funding`)) + geom_point() + geom_smooth()

ggplot(df, aes(Hispanic, Highschool)) + geom_point() + geom_smooth()
ggplot(df, aes(Hispanic, Ratio)) + geom_point() + geom_smooth()
ggplot(df, aes(Hispanic, `Perkins funding`)) + geom_point() + geom_smooth()

ggplot(df, aes(White, Highschool)) + geom_point() + geom_smooth()
ggplot(df, aes(White, Ratio)) + geom_point() + geom_smooth()
ggplot(df, aes(White, `Perkins funding`)) + geom_point() + geom_smooth()

head(df)

ggplot(df, aes(`Union density`, Highschool)) + geom_point() + geom_smooth()
ggplot(df, aes(`Union density`, `Perkins funding`)) + geom_point() + geom_smooth()

data = na.omit(data)

regress.high.B = lm(Highschool~Black, data = data)
regress.Union.B = lm(`Union density`~Black, data = data)
residuals.high.B = residuals(regress.high.B)
residuals.Union.B = residuals(regress.Union.B)
plot(residuals.high.B,residuals.Union.B)
qqplot(qnorm(seq(0,1,length=101),mean(residuals.high.B), sd(residuals.high.B)),residuals.high.B)
partial.correlation.B = cor(residuals.high.B, residuals.Union.B, method = 'spearman')
n = length(data$Highschool)
test.statistic.B = partial.correlation.B*sqrt((n-3)/(1-partial.correlation.B^2))
p.value.B = 2*pt(abs(test.statistic.B), n-3, lower.tail = F)


regress.high.W = lm(Highschool~White, data = data)
regress.Union.W = lm(`Union density`~White, data = data)
residuals.high.W = residuals(regress.high.W)
residuals.Union.W = residuals(regress.Union.W)
plot(residuals.high.W,residuals.Union.W)
qqplot(qnorm(seq(0,1,length=101),mean(residuals.high.W), sd(residuals.high.W)),residuals.high.W)
partial.correlation.W = cor(residuals.high.W, residuals.Union.W, method = 'spearman')
test.statistic.W = partial.correlation.W*sqrt((n-3)/(1-partial.correlation.W^2))
p.value.W = 2*pt(abs(test.statistic.W), n-3, lower.tail = F)


regress.high.H = lm(Highschool~Hispanic, data = data)
regress.Union.H = lm(`Union density`~Hispanic, data = data)
residuals.high.H = residuals(regress.high.H)
residuals.Union.H = residuals(regress.Union.H)
plot(residuals.high.H,residuals.Union.H)
qqplot(qnorm(seq(0,1,length=101),mean(residuals.high.H), sd(residuals.high.H)),residuals.high.H)
partial.correlation.H = cor(residuals.high.H, residuals.Union.H, method = 'spearman')
test.statistic.H = partial.correlation.H*sqrt((n-3)/(1-partial.correlation.H^2))
p.value.H = 2*pt(abs(test.statistic.H), n-3, lower.tail = F)

