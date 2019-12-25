#####################
# Var1
#####################
# This is cod from github.com/MarkEdmondson1234/googleMeasureR
# devtools::install_github("MarkEdmondson1234/googleMeasureR")

library(googleMeasureR)
library(httr)

googleMeasureR::gmr_post(list(v = 1, 
                              t = 'event', 
                              tid = 'UA-119903076-1', 
                              cid = '1960503084.1571678748', 
                              ec = 'test5'))
# function googleMeasureR return TRUE

#####################
# Var2
#####################
# POST-request. It works!
# /batch don't work
# https://developers.google.com/analytics/devguides/collection/protocol/v1/devguide#commonhits
httr::POST(
  "https://www.google-analytics.com/collect",
  body = payload_data,
  encode = "form" #important
  )

payload_data <- 
  list(v=1,t='event',tid='UA-119903076-1',cid='1960503084.1571678748',ec='test8')
