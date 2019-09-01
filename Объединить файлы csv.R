?dir
dir(pattern = "*.csv")

#Создаем пустой датафрейм
grands <- data.frame()

for (i in dir(pattern = "*.csv")){
  temp_df <- read.csv(i)
  grants <- rbind(temp_df, grants)
}
?rbind

#Объединить файлы csv в корневой папке
read_data <- function() {
  grands <- data.frame()
  number <- 0
  for (i in dir(pattern = "*.csv")){
    temp_df <- read.csv(i)
    grants <- rbind(temp_df, grants)
    number <- number+1
  }
  print(paste(as.character(number), "файлов объединилось"))
  return(grands)
  
}

#Вызвать функцию из другого файла
source("Объединить файлы csv.R")