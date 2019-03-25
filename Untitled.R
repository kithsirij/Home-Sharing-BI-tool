library(shinydashboard)
library(leaflet)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) # make the jsonlite suggested dependency explicit
library(plotly)

#setwd("~/Documents/Courses/STAT425/Project/")

AirSL <- read.csv("listings.csv",stringsAsFactors = F)
AirSL$X <- NULL
#AirSL$host_since <- as.integer(AirSL$host_since/30)
library(dplyr)
a <- AirSL %>% group_by(neighbourhood_cleansed) %>% summarise(len = length(neighbourhood_cleansed))
sum(a[(a$len<70),2])

AirSL <- merge(AirSL,a,by = "neighbourhood_cleansed")
AirSL$neighbourhood_cleansed <- ifelse(AirSL$len<150,"Others",AirSL$neighbourhood_cleansed)
##########binning############################################################################

AirSL$accommodates_bin <- ifelse(AirSL$accommodates>=1 & AirSL$accommodates<=3,
                                   "1 to 3", ifelse(AirSL$accommodates>3 & AirSL$accommodates<=6,
                                                    "4 to 6", ifelse(AirSL$accommodates>6 & AirSL$accommodates<=9,
                                                                     "6 to 9", "More than 9")))
AirSL$bedrooms_bin <- ifelse(AirSL$bedrooms>=0 & AirSL$bedrooms<=1,
                               "0 to 1", ifelse(AirSL$bedrooms>1 & AirSL$bedrooms<=2,
                                                "1 to 2", "More than 2"))
AirSL$bathrooms_bin <- ifelse(AirSL$bathrooms>=0 & AirSL$bathrooms<=1,
                                "0 to 1", ifelse(AirSL$bathrooms>1 & AirSL$bathrooms<=2,
                                                 "1 to 2", "More than 2"))


