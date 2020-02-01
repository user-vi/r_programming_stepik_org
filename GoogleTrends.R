# https://www.rubenvezzoli.online/bulk-download-data-google-trends-r/
# https://sites.google.com/site/tomihasa/google-language-codes

# Install and load the readr and gtrendsR packages

# install.packages("readr","gtrendsR")
library(readr)
library(gtrendsR)

# Load your keywords list (.csv file)

# kwlist = readLines("Your-keywords-list-path.csv")
kwlist = c("купить ноутбук", "какой ноутбук")

# The for loop downloads the data for every single keyword in your list

resultslist <- list() 
# 
# for (keywords in kwlist){
#   
#   # Set the geographic region, time span, Google product,... 
#   # for more information read the official documentation https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf
#   country=c('RU')
#   time=("2018-08-01 2018-08-27")
#   # channel='web'
#   trends = gtrends(keywords, gprop =channel, geo=country, time = time )
#   resultslist[[keywords]] <- trends$interest_over_time
#   
# }

for (keywords in kwlist){
  
  # Set the geographic region, time span, Google product,... 
  # for more information read the official documentation https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf
  country=c('RU')
  time=("2005-08-01 2019-12-31")
  # channel='web'
  trends = gtrends(keywords, geo=country, time = time )
  resultslist[[keywords]] <- trends$interest_over_time
  
}

# Merge all the data into a dataframe
# output <- as.data.frame(do.call("rbind", resultslist)) 

# Download the dataframe "output" as a .csv file
# write.csv(output,"download.csv")


# попробуем ариму
library(dplyr)
df <- as_data_frame(output)
df$date <- as.Date(df$date)

t <- df %>% 
  filter(date > "2008-01-01") %>% 
  filter(date < "2018-01-01") %>%
  group_by(date) %>% 
  summarise(hits = sum(hits))

# plot(df$hits~df$date, type = "l")
plot(t$hits~t$date, type = "l")



# Convert date to table with frequency
tt <- as.data.frame(table(t$date))

ts <- ts(t$hits, 
         start = c(2008,1),
         frequency=12)

library(forecast)
# frequency see in help.Detail
fit_arima <- auto.arima(ts)
plot(forecast(fit_arima,h=24))
pred <- forecast(fit_arima, h = 24)
# AIC(fit_arima) 
plot(decompose((ts), type="multiplicative"), yaxt='n')

temp <- as_data_frame(pred$lower)

  

library(forecast)
#Построение модели
ts <- ts(t$hits, start = c(2008,1), frequency = 12)
fit_hw <- hw(ts, h = 24, seasonal = "multiplicative", level = 0.90)
# AIC(fit_hw) 1180 см в fit_hw
predict_values <- round(model[2]$mean)
#Сохранение графиков
plot_hw <- plot(model, xlim = c(2017,2021), ylim = c(0,200), main="Поисковые запросы", ylab="Факт / Прогноз",xlab="Годы")


library(prophet)
t_fb <- t %>% 
  rename(ds = date, y = hits)
m <- prophet(t_fb, 
             seasonality.mode = "multiplicative")
future <- make_future_dataframe(m, periods = 15, freq = 'month')
tail(future)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)



prophet_plot_components(m, forecast)
