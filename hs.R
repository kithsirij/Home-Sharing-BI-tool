library(car)
library(dplyr)
library(psych)
library(dplyr)
detach("package:plyr", unload=TRUE) 
library(leaflet)
library(gender)
library(rockchalk)
library(gdata)
library(plyr)
library(corrplot)
library(gmodels)
library(ggplot2)
library(ggmap)


setwd("C:/Users/Methun Perera/Desktop/HSharingXYZ/Shiny")


AirSL = read.csv("listings.csv")


str(AirSL)



AirSL$price <- as.numeric(sub("\\$", "", AirSL$price)) 

#AirSL$bed_type <- as.factor(AirSL$bed_type)
#table(AirSL$bed_type)

AirSL$room_type <- as.factor(AirSL$room_type)
table(AirSL$room_type)



AirSL$neighborhood <- as.factor(AirSL$neighborhood)
table(AirSL$neighborhood)

#AirSL$zipcode <- as.factor(AirSL$zipcode)
#table(AirSL$zipcode)

AirSL$neighborhood <- as.factor(AirSL$neighborhood)
table(AirSL$neighborhood)

#AirSL$cancellation_policy <- as.factor(AirSL$cancellation_policy)
#table(AirSL$cancellation_policy)

#AirSL$Multiple_Host <- as.factor(AirSL$Multiple_Host)
#table(AirSL$Multiple_Host)


### Minimum Nights(2 Levels: 1 and More than 1):
AirSL$minstay<- as.numeric(AirSL$minimum_nights)
AirSL$minstay[AirSL$minstay==1 ] <- "1"
AirSL$minstay[AirSL$minstay > 1] <- "More than 1"
AirSL$minstay <-as.factor(AirSL$minstay)
table(AirSL$minstay)

### accomodates(2 Levels: Atmost 2 and More than 2):
AirSL$accommodates<- as.character(AirSL$accommodates)
AirSL$accommodates<- as.factor(AirSL$accommodates)
AirSL$accommodates<-recode(AirSL$accommodates, `1` = "Atmost 2",`2` = "Atmost 2", .default ="More than 2")
table(AirSL$accommodates)

### Bathrooms(2 Levels: 1 and More than 1):
AirSL$bathrooms<- as.character(AirSL$bathrooms)
AirSL$bathrooms<- as.factor(AirSL$bathrooms)
AirSL$bathrooms<-recode(AirSL$bathrooms, `1` = "1", .default ="More than 1")
table(AirSL$bathrooms)

### Bedrooms(2 Levels: 1 and More than 1):
AirSL$bedrooms<- as.character(AirSL$bedrooms)
AirSL$bedrooms<- as.factor(AirSL$bedrooms)
AirSL$bedrooms<-recode(AirSL$bedrooms, `1` = "1", .default ="More than 1")
table(AirSL$bedrooms)


##Room Type: 
ggplot(AirSL, aes(x=room_type, fill= room_type)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Property Type", x="Room Type", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Room Type")

##Accomodates: 
ggplot(AirSL, aes(x=accommodates, fill= accommodates)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Accommodates", x="Accommodates", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Accommodates")

##Bathrooms: 
ggplot(AirSL, aes(x=bathrooms, fill= bathrooms)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Bathrooms", x="Bathrooms", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Bathrooms")

##Bedrooms:
ggplot(AirSL, aes(x=bedrooms, fill= bedrooms)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Bedrooms", x="Bedrooms", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Bedrooms")

##Minimum Nights:
ggplot(AirSL, aes(x=minstay, fill= minstay)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Minimum Nights", x="Minimum Nights", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Minimum Nights")


##Locations:
ggplot(AirSL, aes(x=neighborhood, fill= neighborhood)) + 
  geom_bar(position="dodge") + 
  labs(title="Univariate Analysis: Locations", x="Locations", y="Count") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Locations")



##Number of Reviews:

hist(AirSL$reviews, main="Distribution of Number of Reviews", 
     col="steelblue", freq=F, ylim = c(0,0.07), xlab = "Number of Reviews")

lines(density(AirSL$reviews, na.rm = T), col="orange", lwd=3)

##Log Transformation-Number of Reviews:
hist(log(AirSL$reviews), main="Log Transformation: Distribution of Number of Reviews", 
     col="steelblue", freq=F, ylim = c(0,0.4), xlab = "Log(Number of Reviews)")
lines(density(log(AirSL$reviews), na.rm = T), col="orange", lwd=3)

##Price[DEPENDENT VARIABLE]:

hist(AirSL$price, main="Distribution of Price", 
     col="steelblue", freq=F, ylim = c(0,0.01), xlab = "Price")
lines(density(AirSL$price, na.rm = T), col="orange", lwd=3)

##Log Transformation-Price:
hist(log(AirSL$price), main="Log Transformation: Distribution of Price", 
     col="steelblue", freq=F, ylim = c(0,0.8), xlab = "Log(Price)")
lines(density(log(AirSL$price), na.rm = T), col="orange", lwd=3)


## Price Distribution by Locations:
ggplot(data=AirSL, aes(x=log(price), fill=neighborhood))+geom_density(alpha=.8) +
  labs(title="Price Distribution by Locations", x="Log(Price)", y="Density") +  
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Locations")

## Distribution of Number of Reviews by neighborhood:
ggplot(data=AirSL, aes(x=log(reviews), fill=neighborhood)) +
  geom_density(alpha=.8) +
  labs(title="Distribution of Number of Reviews by Locations", x="log(Number of Reviews)", y="Density") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Locations")


## Relationship between  Price and Number of Reviews in different Regions of Chicago: 
ggplot(data=AirSL, aes(x=log(reviews), y=log(price))) + 
  geom_point() + 
  facet_grid(.~neighborhood) +
  stat_smooth(method="lm", se=F, lwd=2) + 
  labs(title="Relationship between Price and Number of Reviews in different Regions of Chicago", x="Log(Number of Reviews)", y="Log(Price)") +  
  theme(plot.title = element_text(hjust = 0.5))



## Hypothesis 1: 
### Checking for conditions of ANOVA
AirSL %>% group_by(neighborhood)%>%summarise(Average= mean(reviews, na.rm= TRUE), Median= median(reviews, na.rm=TRUE),SD= sd(reviews, na.rm=TRUE),Variance= var(reviews,na.rm = TRUE))
### ANOVA Test:
aov(log(AirSL$reviews)~AirSL$neighborhood)
summary(aov(log(AirSL$reviews)~AirSL$neighborhood))
TukeyHSD(aov(log(AirSL$reviews)~AirSL$neighborhood),conf.level = 0.99)



##Correlation Matrix 1 (Hypothesis 2):
AirSL_test <- AirSL %>% filter(!is.na(AirSL$price))#Filter NA Values of Price
AirSLnum<- AirSL_test[,c(16,21,22,27,28)]
cormat <- cor(AirSLnum)
corrplot(cormat, addCoef.col = "black", method = "square", type="upper", diag=FALSE, title= "Correlation Matrix",mar=c(0,0,4,0))


##Correlation Test between price and number of reviews
cor.test(AirSL$price,AirSL$number_of_reviews)

##Correlation Matrix 2:
AirSL_avail<- AirSL_test[,c(18,19,20,21)]# Correlation Matrix of Availability
cormat_avail <- cor(AirSL_avail)
corrplot(cormat_avail, addCoef.col = "black", method = "square", type="upper", diag=FALSE, title= "Correlation Matrix: Availability",mar=c(0,0,4,0))


testmap <- get_googlemap(c(lon=80.26478,lat=6.00468) ,zoom=6, 
                         xlim=c(127,142), ylim=c(-23,-34))

ggmap(testmap) + geom_point(aes(x=longitude, y=latitude), 
                            data=AirSL, colour="red", size=0.00001)

