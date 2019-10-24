# devtools::install_github("MarkEdmondson1234/googleMeasureR")

library(googleMeasureR)

googleMeasureR::gmr_post(list(v = 1, t = 'event', tid = 'UA-119903076-1', cid = '1960503084.1571678748', ec = 'test5'))
# function return TRUE