"Основы R. Часть 1. Логистическая регрессия"
#############################

library(ggplot2)

my_df <- read.csv("https://stepik.org/media/attachments/lesson/10226/train.csv", sep=";")
str(my_df)

ggplot(my_df, aes(read, math, col = gender))+
  geom_point(size = 5)+
  facet_grid(.~hon)+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))


fit  <- glm(hon ~ read + math + gender, my_df, family = "binomial")
summary(fit)

exp(fit$coefficients)

head(predict(object = fit))

head(predict(object = fit, type = "response"))


my_df$prob  <- predict(object = fit, type = "response")

#упражнение1
#Используем данные mtcars. Сохраните в переменную логистическую регрессионную модель, 
#где в качестве зависимой переменной выступает тип коробки передач (am), 
#в качестве предикторов переменные disp, vs, mpg.
#Значения коэффициентов регрессии сохраните в переменную log_coef.

fit_mtcars  <- glm(am ~ disp + vs+ mpg, mtcars, family = "binomial")
log_coef <- fit_mtcars$coefficients


#Упражнение2
library("ggplot2")

ggplot(data = ToothGrowth, aes(x=supp, y = len, fill = dose))+
  geom_boxplot()

View(ToothGrowth) 
df <- ToothGrowth
str(df)


############################################


library(ROCR)

pred_fit <- prediction(my_df$prob, my_df$hon)
perf_fit <- performance(pred_fit,"tpr","fpr")
plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.1))
auc  <- performance(pred_fit, measure = "auc")
str(auc)



perf3  <- performance(pred_fit, x.measure = "cutoff", measure = "spec")
perf4  <- performance(pred_fit, x.measure = "cutoff", measure = "sens")
perf5  <- performance(pred_fit, x.measure = "cutoff", measure = "acc")

plot(perf3, col = "red", lwd =2)
plot(add=T, perf4 , col = "green", lwd =2)
plot(add=T, perf5, lwd =2)

legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)

abline(v= 0.225, lwd = 2)


my_df$pred_resp  <- factor(ifelse(my_df$prob > 0.225, 1, 0), labels = c("N", "Y"))

my_df$correct  <- ifelse(my_df$pred_resp == my_df$hon, 1, 0)


ggplot(my_df, aes(prob, fill = factor(correct)))+
  geom_dotplot()+
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=25,face="bold"))
  
mean(my_df$correct)


test_df  <- read.csv("test.csv", sep = ";")
test_df$hon  <- NA

test_df$hon  <- predict(fit, newdata = test_df, type = "response")
View(test_df)


#########################################################################
"Основы статистики. Часть 2"

"Начнем с простого и вспомним, как применять логистическую регрессию в R. 
Напишите функцию get_coefficients, которая получает на вход dataframe 
с двумя переменными x ( фактор с произвольным числом градаций) 
и y ( фактор с двумя градациями). Функция строит логистическую модель, 
где y — зависимая переменная, а x — независимая, и возвращает вектор 
со значением экспоненты коэффициентов модели. "

test_data <- read.csv("https://stepik.org/media/attachments/course/524/test_data_01.csv")
test_data  <- transform(test_data, x = factor(x), y = factor(y)) 
simple_fit <- glm(test_data$y~test_data$x, test_data, family = "binomial")
total <- c(exp(simple_fit$coefficients[1]),
               exp(simple_fit$coefficients[2]),
               exp(simple_fit$coefficients[3]))
exp(simple_fit$coefficients)

get_coefficients <- function(dataset) {
  dataset  <- transform(dataset, x = factor(x), y = factor(y))  
  simple_fit <- glm(dataset$y~dataset$x, dataset, family = "binomial")
  total <- exp(simple_fit$coefficients)
  return(total)
}

get_coefficients(test_data)

"Интересной особенностью логистической регрессии является тот факт, 
что ее предсказания — это не конкретный класс, к которому мы отнесем новое наблюдение, 
а вероятность отнесения к каждому из классов! Если вас интересует, 
как принимать решение о классификации новых объектов в логистической регрессии, 
посмотрите наш урок по этой теме в курсе по R, где мы разбираем этот вопрос.

В результате, построив регрессионную модель, мы можем сделать 
вероятностное предсказание для каждого нового наблюдения. 
Иногда при решении практических задач бывает важным обратить внимание на те объекты, 
которые получили максимальное значение вероятности принадлежности к одному из классов.

Продолжим нашу работу в службе безопасности! Разобравшись с тем, 
какие предикторы могут помогать нам предсказывать запрещенный багаж, 
давайте применим наши знания для повышения безопасности в аэропорту. 
Обучим наш алгоритм различать запрещенный и разрешенный багаж 
на уже имеющихся данных и применим его для сканирования нового багажа!

Напишите функцию, которая принимает на вход два набора данных. 
Первый dataframe, как и в предыдущей задаче, содержит информацию об уже осмотренном багаже
(запрещенный или нет, вес, длина, ширина, тип сумки). 

Второй набор данных — это информация о новом багаже, который сканируется прямо сейчас. 
В данных также есть информация:  вес, длина, ширина, тип сумки и имя пассажира 
(смотри описание переменных в примере). 

Используя первый набор данных, обучите регрессионную модель различать запрещенный 
и разрешенный багаж. При помощи полученной модели для каждого наблюдения 
в новых данных предскажите вероятность того, что багаж является запрещенным. 
Пассажиров, чей багаж получил максимальное значение вероятности, 
мы попросим пройти дополнительную проверку. 

Итого, ваша функция принимает два набора данных и возвращает имя пассажира 
с наиболее подозрительным багажом. Если несколько пассажиров получили 
максимальное значение вероятности, то верните вектор с несколькими именами. 

В этой задаче для предсказания будем использовать все предикторы, 
даже если некоторые из них оказались незначимыми. 
Для предсказания стройте модель без взаимодействия предикторов."

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_passangers.csv")
data_for_predict <-read.csv("https://stepic.org/media/attachments/course/524/predict_passangers.csv")

simple_fit <- glm(is_prohibited ~ ., test_data, family = "binomial")

summary(simple_fit)

anova(simple_fit, test = "Chisq")
df <- data_for_predict
df$predict <- predict(simple_fit, df)
subset(df, predict>1.38, select=passangers)


most_suspicious <- function(test_data, data_for_predict){
  simple_fit <- glm(is_prohibited ~ ., test_data, family = "binomial")
  data_for_predict$predict <- predict(simple_fit, df)
  return (data_for_predict[which.max(data_for_predict$predict),5])
}

most_suspicious(test_data, data_for_predict)
