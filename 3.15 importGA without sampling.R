# start_date <- '2020-04-11'
# end_date <- '2020-05-11'
# View <-  'ga:9325540'
# metrics <- "ga:newUsers, ga:sessions"
# dimensions <- "ga:date, ga:dimension4, ga:pagePath, ga:dimension5"
# df <- import_without_sampling(start_date, end_date, View, metrics, dimensions)


import_without_sampling <- function(start_date, end_date, View, metrics, dimensions){
  #### start function for import from GA without sampling ####
  # date start and finish
  temp <- seq(as.Date(start_date), as.Date(end_date), 'days')
  
  
  # create empty dataFrame
  gaData <- data_frame()
  
  # log time 
  start_time <- Sys.time()
  
  for (i in 1:length(temp)) {
    start_time_iter <- Sys.time()
    gaData_temp <- RGA::get_ga(profileId = View,
                               start.date = temp[i],
                               end.date = temp[i],
                               metrics     = metrics,
                               dimensions  = dimensions)
    
    
    # gaData <- rbind(gaData,gaData_temp)
    gaData <- gaData %>% 
      bind_rows(gaData_temp)
    end_time_iter <- Sys.time()
    
    print(paste(i, '/', length(temp), end_time_iter - start_time_iter, "/", (length(temp) - i) * (end_time_iter - start_time_iter)))
  }
  
  rm(gaData_temp)
  
  # log time 
  end_time <- Sys.time()
  end_time - start_time
  
  return(gaData)
  #### end function for import from GA without sampling ####
  
  
}