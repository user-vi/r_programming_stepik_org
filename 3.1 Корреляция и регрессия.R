df3 <- mtcars

#Посчитать коэф корреляции
#По умолчанию счит коэф Пирсона для норм распределения, 
#можно установить method = "spearman" для ненорм распределения
cor_test_value <- cor.test(x = df$mpg, y = df$hp) 
cor_test_value$p.value
cor_test_value[[3]]
#Обязательно визуилизировать данные на графике
plot(x = df3$mpg, y = df3$hp)
#или
library(ggplot2)
ggplot(df, aes(x = df$mpg, y = df$hp))+
  geom_point(size = 5)+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)


#########################################
#Регресионный анализ с несколькими предикторами (мультиколленеарность)
#Парный анализ на графике скотер плот
pairs(df[, c(1,3:7)])
#и посчитать таблицу с коэф корреляции
cor(df[, c(1,3:7)])
#или посчитать коэф корреляции и р-значение
library(psych)
fit <- corr.test(df[, c(1,3)])
fit$r #коэф корреляции
fit$p #р-значение

#########################################
#Построение регресионной модели
#создадим сравнительную таблицу с реальными значениями и предсказанными
fit1 <- lm(mpg~hp, df)
fitted_values_mpg <- data.frame(mpg = df$mpg, fitted = fit1$fitted.values)
?lm    

fit_iris <- lm(iris[,c(1,4)])
fitted_values_iris <- data.frame(fitted = fitiris$fitted.values)
fit_iris$fitted.values

#Предскажем значения mpg для новых значений hp = 100, 150, 129, 300
new_hp <- data.frame(hp = c(100,150,129,300))                               
new_hp$mpg <- predict(fit1, new_hp)  

#########################################
#Упражнение
#постройте линейную регрессию, где - первая переменная - зависимая, вторая - независимая. 
#В ответ укажите значения регрессионных коэффициентов сначала intercept затем  slope.
df2 <- read.table("dataset_11508_12.txt")
fit2 <- lm(V1~V2, df2)
summary(fit2)

#Упражнение2
#Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. 
#Только для бриллиантов класса Ideal (переменная cut) c числом карат равным 0.46 
#(переменная carat) постройте линейную регрессию, 
#где в качестве зависимой переменной выступает price, в качестве предиктора - переменная  depth. 
#Сохраните коэффициенты регрессии в переменную fit_coef.

fit3 <- lm(price~depth, diamonds[c(diamonds$carat == 0.46 & diamonds$cut == "Ideal"), ])
fit3$coefficients

#Упражнение3
#Постройте scatterplot по данным iris, сохранив его в переменную my_plot : 
#Ось X - переменная Sepal.Width
#Ось Y -  переменная Petal.Width
#Цвет точек - переменная Species
#Также добавьте линейное сглаживание для каждой группы наблюдений по переменной Species.
library(ggplot2)
my_plot <- ggplot()


ggplot(iris, aes(x = iris$Sepal.Width, y = iris$Petal.Width, col = Species))+
  geom_point()+
  geom_smooth(method = "lm")

#Упражнение4
#Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
#Если две переменные значимо коррелируют 
#(p - уровень значимости для коэффициента корреляции Пирсона меньше 0.05), 
#то функция строит регрессионную модель, 
#где первая переменная - зависимая, вторая - независимая. 
#Затем создает в dataframe новую переменную с назанием fit, 
#где сохраняет предсказанные моделью значения зависимой переменной. 
#В результате функция должна возвращать исходный dataframe с добавленной новой переменной fit.
#Если две переменные значимо не коррелируют, 
#то функция возвращает строчку "There is no sense in prediction"

regr.calc <- function(df) {
  cor_test_value <- cor.test(x = df[[1]], y = df[[2]])
  if (cor_test_value$p.value < 0.05) {
    fit <- ((lm(df[[1]]~df[[2]], df)$fitted.values))
    return(data.frame(df[[1]], df[[2]], fit))
  }
  else{
    return("There is no sense in prediction")
  }
}
regr.calc(iris[,c(1,4)])

#########################################
########ПАМЯТКА##########################
cor.test(mtcars$mpg, mtcars$disp) # Расчет корреляции Пирсона 

cor.test(~ mpg + disp, mtcars) # запись через формулу

cor.test(mtcars$mpg, mtcars$disp, method = "spearman") # Расчет корреляции Спирмена 

cor.test(mtcars$mpg, mtcars$disp, method = "kendall") # Расчет корреляции Кендала 

cor(iris[, -5]) # построение корреляционной матрицы

fit <- lm(mpg ~ disp, mtcars) # построение линейной регрессии 

fit$coefficients # коэффициенты регрессии 

fit$fitted.values # предсказанные значения зависимой переменной

#в первом aes() будет распространяться на все слои
ggplot(mtcars, aes(mpg, disp, col = factor(am)))+
  geom_point()+
  geom_smooth()

#А то, что в aes() конкретного geom - только на него
ggplot(mtcars, aes(mpg, disp))+
  geom_point(aes(col = factor(am)))+
  geom_smooth()
#А то, что в aes() конкретного geom - только на него
ggplot(mtcars, aes(mpg, disp))+
  geom_point()+
  geom_smooth(aes(col = factor(am)))

########Конец############################
#########################################



