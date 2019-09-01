df <- mtcars[, c(1, 3:7)]
pairs(df)
?pairs
cor(df)
test <- cor.test(df)
test
?cor

#Упражнение
#Кореляция и дисперсия
corr.calc <- function (df){
  library(phych)
  corrInt <- corr.test(df)
  return(c(corrInt$r[1,2], corrInt$p[1,2]))
  
}
#проверка
corr.calc(iris[,1:2])

#Упражнение2
#Напишите функцию smart_cor, которая получает на вход dataframe 
#с двумя количественными переменными. 
#Проверьте с помощью теста Шапиро-Уилка, 
#что данные в обеих переменных принадлежат нормальному распределению.
#Если хотя бы в одном векторе распределение переменной отличается от нормального 
#(p - value меньше 0.05), то функция должна возвращать коэффициент корреляции Спирмена. 
#(Числовой вектор из одного элемента)
smart_cor <- function(df) {
  for (i in 1:2) {
    shapiropvalue <- data.frame()
    temp <- shapiro.test(df[[i]])
    shapiropvalue <- rbind(temp$p.value, shapiropvalue)
  }
  
  for (i in 1:2){
    temp3 <- data.frame()
    if (shapiropvalue(c[i])$p.value < 0.05) {
      temp2 <- C(T)
    }
    else {
      temp2 <- C(F)
    }
    temp3 <- rbind(temp2, temp3)
  }
  if (sum(temp3)==0) {
    pirsontest <- corr.test(df)
    return(pirsontest$p)
  }
  else {
    spearmantest <- corr.test(df, method = "spearman")
    return(spearmantest$p)
  }
}    
  






test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")

smart_cor(test_data)


shapiro.test(test_data$col1)

