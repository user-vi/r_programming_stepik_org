library(dplyr)

# оператор %>% (Ctrl + Shift + m)

data_frame() #dplyr
data.frame() #standart



df <- as_data_frame(mtcars)

# выбрать нужные колонки
select(df, mpg)
select(df, mpg:hp)
select(df, -am)

df <- as_data_frame(iris)
select(df, contains('sepal')) #содержит в названии колонки

# выбрать нужные строки
slice(df, 3:5)
slice(df, c(1,3,5))

# фильтр данных
df <- as_data_frame(mtcars)
filter(df, cyl > 4)

# сортировка
arrange(df, cyl, mpg)

# переименовать колонки
rename(df, new_am = am)
rename(df, new_hp = 4)

# создание вычисл€емой колонки в датафрейм
mutate(df, new_column = seq(0, 1))

# обработка каждой €чейки датафрейма, аналог sapply
# . - точка означает каждую колонку данных 
mutate_each(df, funs(. * 2))
#если значение колонки < 0, то 0, если > 0, то верни значение колонки
mutate_each(df, funs(ifelse(. < 0, 0, .))) 


# группировка + агрегаци€
df %>% 
  group_by(cyl) %>% 
  summarise(mean(mpg))

# сокращенна€ форма
iris %>% 
  filter(Petal.Length > 1.7) %>% 
  arrange(Sepal.Length) %>% 
  select(Sepal.Length, Sepal.Width)

my_df <- mtcars %>% 
  select(mpg, hp, am, vs) %>% 
  filter(mpg > 14 & hp > 100) %>% 
  arrange(desc(mpg)) %>% 
  slice(1:10) %>% 
  rename('Miles per gallon' = mpg, 'Gross horsepower' = hp)


df <- read.csv("https://stepik.org/media/attachments/course/4852/dota_hero_stats.csv")
df
