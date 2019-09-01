#Диагностика модели линейной регрессии

str(swiss)

#Взаимосвязи между переменными
pairs(swiss)

library(ggplot2)
ggplot(swiss, aes(x = Examination, y = Education))+
  geom_point()+
  geom_smooth(method = 'lm') #Добавим линию тренда

ggplot(swiss, aes(x = Examination))+
  geom_histogram()

#Education распределен ненормально
ggplot(swiss, aes(x = Education))+
  geom_histogram()
#можно взять логарифм log(), квадратный корень sqrt() или 1/(Education)
ggplot(swiss, aes(x = log(Education)))+
  geom_histogram()



#Упражнение1
my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 
               0.069, 0.261, 0.241, 0.166, 0.283, 0.041, 0.369, 0.167, 
               0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 
               0.162, 0.152, 0.107, 0.255, 0.037, 0.005, 0.042, 0.220, 
               0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 
               0.265, 0.218, 0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)
shapiro.test(sqrt(my_vector))

#Упражнение2
#Z-преобразование scale()

beta.coef <- function(x){

  result <- lm(scale(x[[1]])~scale(x[[2]]), x)
  return(result$coefficients)
}
beta.coef(mtcars[,c(1,3)])
#############################


#Если зависимость не линейная
ggplot(swiss, aes(x = Examination, y = Education))+
  geom_point()+
  geom_smooth()

#возведем в квадрат независимую переменную:
df <- swiss
fit1 <- lm(Education~Examination, df)
df$Examination_squared <- (df$Examination)^2
fit2 <- lm(Education~Examination+Examination_squared, df)
anova(fit1, fit2)
df$fit1_fitted <- fit1$fitted.values
df$fit2_fitted <- fit2$fitted.values
df$fit1_resid <- fit1$resid 
df$fit2_resid <- fit2$resid
df$obs_number <- 1:nrow(df)

ggplot(df, aes(x = Examination, y = Education))+
  geom_point()+
  geom_line(aes(x = Examination, y = fit1_fitted), col = 'red', lwd = 1)+
  geom_line(aes(x = Examination, y = fit2_fitted), col = 'blue', lwd = 1)

#график остатков, гомоскедастичность
ggplot(df,aes(x = fit1_fitted, y = fit1_resid))+
  geom_point()+
  geom_hline(yintercept=0, col = 'red') #красная линия-предсказанные значения на графике остатков

ggplot(df,aes(x = fit2_fitted, y = fit2_resid))+
  geom_point()+
  geom_hline(yintercept=0, col = 'blue')

#оценить независимость допущения остатков
ggplot(df,aes(x = obs_number, y = fit1_resid))+
  geom_point()+
  geom_smooth()

ggplot(df,aes(x = obs_number, y = fit2_resid))+
  geom_point()+
  geom_smooth()

#гомоскедастичность остатков
ggplot(df,aes(x = fit1_fitted, y = fit1_resid))+
  geom_point()
ggplot(df,aes(x = fit2_fitted, y = fit2_resid))+
  geom_point()

#упражнение
#Функция gvlma() из библиотеки gvlma 
#позволяет получить оценку выполнения основных допущений линейной регрессии. 
#В качестве аргумента она принимает объект, в который сохранена модель. 
#Можно задать формулу модели прямо в функции gvlma. Чтобы увидеть основные статистики, 
#нужно выполнить команду summary для объекта, созданного с помощью функции gvlma. 
my_data <- read.csv('https://stepik.org/media/attachments/lesson/12088/homosc.csv')
library(gvlma)
#################################################

#проверка на нормальность распределения остатков
#модель fit1
ggplot(df,aes(x = fit1_resid))+
  geom_histogram(fill='white', col = 'black')
qqnorm(fit1$residuals)
qqline(fit1$residuals)
shapiro.test(fit1$residuals)
#модель fit2
ggplot(df,aes(x = fit2_resid))+
  geom_histogram(fill='white', col = 'black')
qqnorm(fit2$residuals)
qqline(fit2$residuals)
shapiro.test(fit2$residuals)

#Упражнение
#Напишите функцию resid.norm, которая тестирует распределение остатков 
#от модели на нормальность при помощи функции shapiro.test и создает гистограмму 
#при помощи функции ggplot() с красной заливкой "red", 
#если распределение остатков значимо отличается от нормального (p < 0.05), 
#и с зелёной заливкой "green" - если распределение остатков значимо не отличается от нормального. 
#На вход функция получает регрессионную модель. Функция возвращает переменную, 
#в которой сохранен график ggplot.


resid.norm  <- function(fit){
  shapiro_p_value <- shapiro.test(fit$residuals)
  if (shapiro_p_value$p.value > 0.05){
    plot1 <- ggplot(fit,aes(x = fit$residuals))+
      geom_histogram(fill='green')
    return(plot1)
  }
  else{
    plot2 <- ggplot(fit,aes(x = fit$residuals))+
      geom_histogram(fill='red')
    return(plot2)
  }
}

