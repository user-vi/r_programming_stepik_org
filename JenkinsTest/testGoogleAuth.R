


# library(RGA)
# library(dplyr)
# 
# gaStartDate <- "2019-11-01"
# gaFinishDate <- "2019-11-05"
# View <- "ga:176022541"
# 
# gaDataSession <- RGA::get_ga(profileId = View,
#                              start.date    = gaStartDate,
#                              end.date      = gaFinishDate,
#                              dimensions  = "ga:date",
#                              metrics     = "ga:users")

googlesheets4::sheets_deauth()

# upload table from G.Sheets
data_1 <- googlesheets4::read_sheet("14bwjhoCegcFKdBYrWquoAWqGIFWdOXf2MbpXzcqzXEU", sheet = "plan")

write.csv(data_1, "data_1.csv")

write("testGoogleAuth - success", stdout())



