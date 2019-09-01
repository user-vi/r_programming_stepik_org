View(swiss)
df <- swiss
str(df)
hist(df$Fertility)
fit <- lm(Fertility~Examination+Catholic, df) #влияние Examination и Catholic на Fertility
summary(fit)

fit2 <- lm(Fertility~Examination*Catholic, df) 
#влияние Examination и Catholic на Fertility (И) взаимовлияние Examination и Catholic

summary(fit2)

#доверительный интервал
confint(fit2)

#Упражнение
#Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
#x_1  -  числовой вектор
#x_2 - числовой вектор
#y - числовой вектор с пропущенными значениями.
#Теперь — самое интересное На первом этапе, 
#используя только наблюдения, в которых нет пропущенных значений, 
#мы построим регрессионную модель (без взаимодействий), 
#где  y — зависимая переменная, x_1 и x_2 — независимые переменные. 
#Затем, используя построенную модель, мы заполним пропущенные значения предсказаниями модели.
#Функция должна возвращать dataframe c новой переменной  y_full. 
#Сохраните в нее переменную y, в которой пропущенные значения заполнены предсказанными значениями 
#построенной модели.

test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")

fill_na <- function(df){
  df$y_full <- df[[3]]
  fit <- lm(df[[3]] ~ df[[1]] + df[[2]], df)
  df$y_full[is.na(df$y_full)] <- fit$fitted.values
  full <- data.frame(x_1=df[[1]], x_2=df[[2]], y=df[[3]], y_full = df[[4]])
  return(full)
}

fill_na(test_data)


#Упражнение2
View(mtcars)
df2 <- subset(mtcars, select = c ("wt", "mpg", "disp", "drat", "hp"))
fit4 <- lm(wt~mpg+disp+drat+hp, df2)
fit4 <- lm(wt~mpg+disp+hp, df2)
summary(fit4)

#Упражнение3
df3 <- attitude
fit5 <- lm(rating~complaints*critical,df3)
summary(fit5)

#категориальный предиктор
hist(df$Catholic)
df$religions <- ifelse(df$Catholic>60, 'Lots', 'Few')
df$religions <- as.factor(df$religions)
fit <- lm(Fertility~Examination+religions, df) #влияние Examination и religions на Fertility
summary(fit)

library(ggplot2)
ggplot(df, aes(x = Examination, y = Fertility, col = religions)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

df <- swiss
fit_full <- lm(Fertility~., df)
summary(fit_full)

fit1 <- lm(Fertility~Infant.Mortality+Examination +Catholic+Education, df)
summary(fit1)

#определим, две модели одинаково объясняют дисперсию (р>0.05) или отличаются (р<0.05)
anova(fit_full, fit1)

#чтобы не применять anova и не сравнивать дисперсии:  
optimal_fit <- step(fit_full, direction = 'backward')

#упражнение
model_full <- lm(rating ~ ., data = attitude) 
model_null <- lm(rating ~ 1, data = attitude)
scope = list(lower = model_null, upper = model_full)
optimal_fit <- step(model_full, direction = 'backward')
summary(optimal_fit)
anova(model_full, optimal_fit)

#упражнение
model <- lm(sr ~ ., LifeCycleSavings)

dimnames(HairEyeColor)
?prop.table
dws <- HairEyeColor[, "Green", "Female"]
sum(dws)

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)
ggplot(data = mydata, aes(x = Hair , y = Freq, col = Eye)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))

