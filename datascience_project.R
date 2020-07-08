library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidyr)
library(DT)
library(scales)
library(tidyverse)
library(boxplotdbl)
library(sp)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(rworldmap)
library(caret)
library(rpart.plot)
library(maps)
library(geoR)
library(cholera)


#get the directory
print(getwd())
setwd("C:/Users/dell/Documents")
#load the data
pollution<-read.csv("openaq.csv")
print(pollution)
#working for first 500 rows of data to ploting the graphs and plots better 
pol_fil=head(pollution,n=500)
print(pol_fil)
summary(pol_fil)

#uniquely identifying the data for each attribute
unique(pol_fil$city)
unique(pol_fil$value)
unique(pol_fil$parameter)
unique(pol_fil$location)
unique(pol_fil$country)
unique(pol_fil$utc)
unique(pol_fil$local)
unique(pol_fil$unit)
unique(pol_fil$latitude)
unique(pol_fil$longitude)
unique(pol_fil$attribution)

#ploting to summarising the dataset and more understand the data
#AQI for different cities
ggplot(data = pol_fil) + geom_point(mapping = aes(x = city, y = value),color="brown")
#latitude for different cities
ggplot(data = pol_fil) + geom_point(mapping = aes(x = city, y = latitude),color="brown")
#longitude for different cities
ggplot(data = pol_fil) + geom_point(mapping = aes(x = city, y = longitude),color="brown")
#AQI for different cities according to both parameters
ggplot(data = pol_fil) + geom_point(mapping = aes(x = city, y = value, size = parameter))
#latitude for different cities according to both parameters
ggplot(data = pol_fil) + geom_point(mapping = aes(x = city, y = latitude, size = parameter),color = "blue")
#longitude for different cities according to both parameters
ggplot(data = pol_fil) + geom_point(mapping = aes(x = city, y = longitude, size = parameter),color = "red")



#frequency of AQI in different cities
ggplot(data = pol_fil) +geom_bar(mapping = aes(x = city, y = value), stat = "identity")


#summarising the y values for each unique x value in which maximum is for chennai
ggplot(data = pol_fil) + 
  stat_summary(
    mapping = aes(x = city, y = location),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
#polluted cities according to parameteres
ggplot(data = pol_fil) + geom_bar(mapping = aes(x = city, fill = parameter))

#pollution in cities which has AQI<50 according to their value,latitude and longitude
top50_pol_cities<-pol_fil[order(-pol_fil$value),][1:50,]
top50_pol_cities
hist(top50_pol_cities$longitude,
     main="Maximum daily AQI",
     xlab="longitude",
     col="chocolate",
     border="brown",
     freq=FALSE
)
hist(top50_pol_cities$latitude,
     main="Maximum daily AQI",
     xlab="latitude",
     col="chocolate",
     border="brown",
     freq=FALSE
)
hist(top50_pol_cities$value,
     main="Maximum daily AQI",
     xlab="AQI",
     col="chocolate",
     border="brown",
     freq=FALSE
)
#ploting a graph to show AQI of most polluted two cities according to their AQI values
ggplot(data = top50_pol_cities) + geom_bar(mapping = aes(x = city, colour = city),color="yellow")+geom_point(mapping = aes(x = city, y = value),color="brown")

#ploting a graph to show longitude of most polluted two cities according to their longitude values
ggplot(data = top50_pol_cities) + geom_bar(mapping = aes(x = city, colour = city),color="brown")+geom_point(mapping = aes(x = city, y = longitude),color="yellow")

#ploting a graph to show latitude of most polluted two cities according to their latitude values
ggplot(data = top50_pol_cities) + geom_bar(mapping = aes(x = city, colour = city),color="blue")+geom_point(mapping = aes(x = city, y = latitude),color="red")

# Vertical bar plot for polluted air according to values
barplot(top50_pol_cities$value, main = 'polluted air according to AQI',xlab = 'AQI levels', col= 'brown',horiz = FALSE)

#less polluted locations which has less then 50 AQI
ggplot(data.frame(cbind(pol_fil$value, pollution = pol_fil$value)))+
  geom_point(aes(pol_fil$city, pol_fil$location, color = pollution), size = 2) +
  coord_fixed(ratio = 1) +
  scale_color_gradient(low = "yellow", high = "blue") +
  geom_path(data = data.frame(pol_fil$city), aes(pol_fil$city, pol_fil$location)) +
  theme_bw()

top50_pol_cities1<-sort(top50_pol_cities$local)
top50_pol_cities1
#utc of all cities according to local time which will show tht delhi was the worst city to live in 2018

ggplot(data = top50_pol_cities) + geom_bar(mapping = aes(x = local,colour = city),color="red")+geom_point(mapping = aes(x = value, y = city),color="brown")
#utc of all cities according to local time



#ploting a graph to show AQI in different cities according to their parameters
ggplot(pollution, aes(x = parameter, y = city))+geom_point(aes(color = utc)) +
  geom_line(aes(y = city))

#describing cities
may_to_june_2018<-pol_fil$utc
may_to_june_2018
ggplot(pol_fil, aes(x = may_to_june_2018, y = city)) +
  geom_point(aes(color = city, size =  city))



#coxcombchart between city and location
bar <- ggplot(data = pol_fil) + 
  geom_bar(
    mapping = aes(x = utc, fill = city), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
#morning time has the highest pollution
bar + coord_polar()
unique(pol_fil$city)

#coxcombchart between city and utc
bar <- ggplot(data = pol_fil) + 
  geom_bar(
    mapping = aes(x = city, fill = value),
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_polar()



