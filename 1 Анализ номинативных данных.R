#Импортируем таблицу .csv
df <- read.csv("grants.csv")

#Посмотреть структуру данных
str(df)

#Присвоить категории факторным значениям
df$status <- factor(df$status, labels = c("Not funded", "Funded"))

#Создать локальные таблицы из датасета
t1 <- table(df$status)
View(t1) #Отображает таблицу в отдельной вкладке
dim(t1) #Вывести размер таблицы
t2 <- table(df$status, df$field)
t2 #Отображает табицу в консоли
t2 <- table(status=df$status, field=df$field) #Подписать названия полей
dim(t2) #Вывести размер таблицы в формате: кол-во строк, кол-во столбцов

#Преобразование значений в процент
prop.table(t2, 1) #Сумма в строке 100%
prop.table(t2, 2) #Сумма в столбце 100%

#Создание 3d таблицы
t3 <- table(years=df$years_in_uni, status=df$status, field=df$field)
t3


#Упражнение
dimnames(HairEyeColor)
HairEyeColor
HairEyeColor[ , ,'Male']
red_men <- prop.table(HairEyeColor[, "Blue","Male" ], "Red" )
red_men
HairEyeColor[ , ,'Female']

#Гистограмма столбиками
barplot(t1)
barplot(t2, legend.text = TRUE, args.legend = list(x="topright"), beside = TRUE)

#Моизаичная диаграмма
mosaicplot(t2)

#Упражнение
barplot(HairEyeColor[,,"Female"]) 
#или можно выполнить через ggplot2
library(ggplot2)
mydata <- as.data.frame(HairEyeColor) #Преобразование таблицы в датафрейм
obj <- ggplot(data = mydata, aes(x = Eye, y = Freq)) + 
  geom_bar(stat="identity", position = position_dodge())+
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
####################

#Тест Хи-квадрат
t1
chisq.test(t1)
#Тест критерия Фишера
fisher.test(t2)

#Упражнение
t4 <- HairEyeColor["Brown",,"Female"]
t4
chisq.test(t4)

str(diamonds)
t5 <- table(diamonds$cut, diamonds$color)
main_stat <- chisq.test(t5)
####################
