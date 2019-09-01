#создадим массив и заменим NA на среднее значение
distr1 <- rnorm(100)
distr1[1:30] <- NA
distr1[is.na(distr1)] <-  mean(distr1, na.rm = T)

###функция заменяет пропущенные значения на среднее значение/медиану
my_na_rm <- function(x){
  if (is.numeric(x)){
    stat_test <- shapiro.test(x) 
    if (stat_test$p.value > 0.05){
      x[is.na(x)] <-  mean(x, na.rm = T)
      print("NA заменено на среднее значение")
    }
    else{
      x[is.na(x)] <-  median(x, na.rm = T)
      print("NA заменено на медиану")
    }
    return(x)
  }
  else{
    print("X is not numeric")
  }
}

#Вызвать функцию из другого файла
source("работа с NA значениями.R")