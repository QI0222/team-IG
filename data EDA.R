library(readxl)
library(dplyr)
library(corrplot)


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

df = data %>% select(GDP,Gini,Population,`Union density`,`Cost of living`)
a = cor(df)
corrplot(a)

