df <- iris
str(df)
#Создаем отдельный датасет для исследования
df1 <- subset(df, Species != "setosa")
#Проверяем таблицу
table(df1$Species)
#Построим гистограмму частотности
hist(df1$Sepal.Length)
#Или строим гистограмму частотности через gglpot2
library(ggplot2)
ggplot(df1, aes(x=Sepal.Length))+
  geom_histogram(fill="White", col= "Black", binwidth = 0.4)+
  facet_grid(Species~.)
#Построим график плотности распределения
ggplot(df1, aes(x=Sepal.Length, fill = Species))+
  geom_density(alpha=0.5)
#Строим график бокс-плот
ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

################################################################

#Проверка на нормальность распределения
# но не удобно когда есть группирующая факторная переменная
shapiro.test(df1$Sepal.Length) #Если р-значение больше 0,05, распределение Нормальное

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

#Проверка на гомогенность дисперсии
bartlett.test(Sepal.Length~Species,df1)#Если р-значение больше 0,05, 
#гомогенность соблюдается, отсутствуют различия в дисперсиях

#Т-тест
#для независимых переменных сравнивают фактор и значение (или значение и значение), 
#для независимых переменных
t.test(Sepal.Length~Species,df1) #количественная+фактор
t.test(df1$Petal.Length, df1$Petal.Width) #количественная+количественная

test1 <- t.test(Sepal.Length~Species,df1)
test1$p.value

t.test(Sepal.Length~Species,df1, var.equal = T) #при условии гомогенности дисперсий
#по умолчанию парные данные(paired) = F
#Если р-значение меньше 0,05, отличия имеются

t.test(df1$Sepal.Length, mu=8) #является ли число 8 средним значением выборки Sepal.Length
#Если р-значение меньше 0,05, не является (см доверительный интервал)

#Т-тест для зависимых переменных
t.test(df1$Petal.Length, df1$Petal.Width, paired = T) #по умолчанию парные данные(paired) = F
#Если р-значение меньше 0,05, отличия имеются

################################################################
#Упражнение
str(ToothGrowth)

sub1 <- subset(ToothGrowth, supp == "OJ")
sub2 <- subset(ToothGrowth, supp == "VC")
t_stat <- t.test(sub1$len, sub2$len)["p.value"]
t_stat
################################################################

#Построим графики
install.packages ("Hmisc")
ggplot(data = df1, aes(x = Species, y = Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar")+
  stat_summary(fun.y = mean, geom = "point", size = 4)


#Непараметрический аналог Т-теста. Тест Манна-Уитни

#Для независимых выборок
wilcox.test(Petal.Length~Species, df1) #если меньше чем 0,05, то данные различаются

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()

#Для зависимых выборок, парный тест
wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

################################################################
#Упражнение1 сравнение количественной и факторной переменных
df3 <- read.table("dataset_11504_13.txt")
str(df3)
#Проверка на гомогенность дисперсии
btest <- bartlett.test(V1~V2, df3) #Если р-значение больше 0,05, гомогенность соблюдается
#Если р-значение больше 0,05- используем Т-тест, var.equal = T
if (btest$p.value >0.05){
  #для независимых переменных
  print(t.test(V1~V2, df3,  var.equal = T))
}else{
  #Если р-значение меньше 0,05- используем тест Манна-Уитни
  print(wilcox.test(df3$V1, df3$V2))
}

#Упражнение2 сравнение двух количественных переменных, 
#проверьте гипотезу о равенстве средних этих переменных 
#при помощи t- теста для независимых выборок
df4 <- read.table("dataset_11504_16.txt")
str(df4)
t.test(df4$V1, df4$V2)
mean(df4$V1)
################################################################
