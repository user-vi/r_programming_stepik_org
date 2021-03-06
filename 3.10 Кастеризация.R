df6 <- read.csv(
  "test6.csv",
  header = T,
  sep = ";",
  dec = ","
)
test_data <- df6



#кластерный анализ к-средних
d = (test_data[, 5:6])
# fit <- kmeans(d, 3)
fit <- kmeans(d, 4)
d$clusters <- factor(fit$cluster)
ggplot(d, aes(d[[1]], d[[2]], col = clusters))+
  geom_point(size = 2)+
  theme_bw()


#иерархическая кластеризация

library(ggplot2)
library(ggrepel) #для подписи точек

ggplot(test_data, aes(расход, Доход_RUB, label = labels))+
  geom_point()
# +geom_text_repel()

d = dist(test_data[, 5:6])
fit <- hclust(d, method = "single") #методов много
test_data$clusters <- factor(fit$cluster)
plot(fit, labels = test_data$labels)
rect.hclust(fit, 2) # укажите желаемое число кластеров, сейчас стоит 2

#############
"Интересной особенностью кластерного анализа является тот факт, 
что мы получаем только итоговый ответ, к какому кластеру принадлежит каждое наблюдение. 
Однако мы не знаем, по каким переменным различаются выделенные кластеры. Поэтому, 
если нас интересует не только сам факт того, что мы смогли выделить кластеры в наших данных, 
но мы также хотим понять, чем же они различаются, разумно сравнить кластеры между собой по имеющимся переменным.

Напишите функцию get_difference, которая получает на вход два аргумента: 

test_data — набор данных с произвольным числом количественных переменных.
n_cluster — число кластеров, которое нужно выделить в данных при помощи иерархической кластеризации. 
Функция должна вернуть названия переменных, по которым были обнаружен значимые различия 
между выделенными кластерами (p < 0.05). Иными словами, после того, как мы выделили заданное число кластеров, 
мы добавляем в исходные данные новую группирующую переменную — номер кластера, 
и сравниваем получившиеся группы между собой по количественным переменным при помощи дисперсионного анализа.

Подсказки:

Не забудьте перевести переменную с номером кластера в фактор! 
Вы можете использовать вашу функцию из предыдущего задания.
Для поиска различий используйте ANOVA (функция aov). Давайте договоримся, 
что для наших целей мы не будем проверять данные на соответствие требованиями 
к применению этого критерия и не будем думать о поправке на множественные сравнения."

test_data1 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
test_data2 <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
test_data <- as.data.frame(list(X1 = c(9, 8, 9, 23, 23, 18, 29, 30, 26), X2 = c(8, 12, 9, 22, 22, 23, 26, 31, 29), X3 = c(12, 8, 11, 19, 21, 23, 26, 31, 28), X4 = c(17, 8, 11, 15, 16, 23, 28, 33, 28)))


get_difference<-  function(test_data, n_cluster){
  fit <- kmeans(test_data, n_cluster)
  test_data$clusters <- factor(fit$cluster)
  acc <- vector()
  for (i in 1:ncol(test_data[,-1])) {
    fit <- summary(aov(test_data[,i] ~ clusters,test_data))
    
      if (fit[[1]][["Pr(>F)"]][1] < 0.05) {
        acc <- c(acc, colnames(test_data[i]))
      }
    
  } 
  return(acc)
}

get_difference(df6, 2)
