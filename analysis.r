library(shinydashboard)
library(leaflet)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(plotly)
#setwd("~/Documents/Courses/STAT425/Project/")
airbnb <- read.csv("AirSL.csv",stringsAsFactors = F)
airbnb$X <- NULL
airbnb$price <- as.integer(airbnb$price)
library(dplyr)
a <- airbnb %>% group_by(district) %>% summarise(len = length(district))
sum(a[(a$len<70),2])

airbnb <- merge(airbnb,a,by = "district")
airbnb$district <- ifelse(airbnb$len<150,"Others",airbnb$district)
##########binning############################################################################

airbnb$accommodates_bin <- ifelse(airbnb$accommodates>=1 & airbnb$accommodates<=3,
                                   "1 to 3", ifelse(airbnb$accommodates>3 & airbnb$accommodates<=6,
                                                    "4 to 6", ifelse(airbnb$accommodates>6 & airbnb$accommodates<=9,
                                                                     "6 to 9", "More than 9")))
airbnb$bedrooms_bin <- ifelse(airbnb$bedrooms>=0 & airbnb$bedrooms<=1,
                               "0 to 1", ifelse(airbnb$bedrooms>1 & airbnb$bedrooms<=2,
                                                "1 to 2", "More than 2"))
airbnb$bathrooms_bin <- ifelse(airbnb$bathrooms>=0 & airbnb$bathrooms<=1,
                                "0 to 1", ifelse(airbnb$bathrooms>1 & airbnb$bathrooms<=2,
                                                 "1 to 2", "More than 2"))


