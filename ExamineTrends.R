library(plyr)
library(lubridate)

#Population Data
popdata=read.csv('C:/Users/Carly/Google Drive/University of Utah - Research/Dissertation Research/SystemsModel/Goldsim Documentation/Data/PopulationData.csv')
popdata=na.omit(popdata)

#Streamflow Data
setwd("C:/Users/Carly/Desktop/SnowpackModeling/Snowbedo/data")
citycreek=read.csv('City.csv')
citycreek$date=as.Date(citycreek$date,format="%m/%d/%Y")
citycreek=citycreek[(citycreek$date>="2004-01-01" & citycreek$date<="2013-12-31"),]

citycreek$month=months(citycreek$date)
citycreek$year=year(citycreek$date)

summarycitycreek=as.matrix(with(citycreek, tapply(albedo, list("Month"=month, "Year"=year), mean)))
summarycitycreek=as.data.frame(t(summarycitycreek))
summarycitycreek$year=rownames(summarycitycreek)

plot(x=summarycitycreek$year,y=summarycitycreek$January,xlab="Year",ylab="Albedo",main="Jan",ylim=c(0.1,0.5))
plot(x=summarycitycreek$year,y=summarycitycreek$February,xlab="Year",ylab="Albedo",main="Feb",ylim=c(0.1,0.5))
plot(x=summarycitycreek$year,y=summarycitycreek$March,xlab="Year",ylab="Albedo",main="March",ylim=c(0.1,0.5))
plot(x=summarycitycreek$year,y=summarycitycreek$April,xlab="Year",ylab="Albedo",main="April",ylim=c(0.1,0.5))
plot(x=summarycitycreek$year,y=summarycitycreek$May,xlab="Year",ylab="Albedo",main="May",ylim=c(0.1,0.5))

plot(x=popdata$Year,y=popdata$SaltLakeCity,xlab="Year",ylab="Population")

colnames(summarycitycreek)
