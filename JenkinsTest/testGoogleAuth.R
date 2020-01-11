


library(RGA)
library(dplyr)

gaStartDate <- "2019-11-01"
gaFinishDate <- "2019-11-05"
View <- "ga:176022541"

gaDataSession <- RGA::get_ga(profileId = View,
                             start.date    = gaStartDate,
                             end.date      = gaFinishDate,
                             dimensions  = "ga:date",
                             metrics     = "ga:users")

write("testGoogleAuth - success", stdout())




