library(Snowbedo)
library(imputeTS)
library(ggplot2)


#Read in data with all necessary parameters (and if needed, combine into a single data.frame)
data=read.csv('BigCottonwood.csv')

#Limit dataset to dates with common data
data=limitperiod(data=data,begin="2000-09-30",end="2011-10-01")


#Formatting---------------------------------------------------------------------------------------------------------
#Convert the shortwave radiation from W/m2/day to Wh/m2/day
data$solar_short_Whm2day=data$solar_short*24

#Convert snowcover to a percentage (0-1)
data$snowcover=data$snowcover/100
#Interpolate snowcover data for "cloud free" conditions
data$cloudfreesnowcover=data$snowcover
data[(data$cloudcover>25),"cloudfreesnowcover"]=NA
data$cloudfreesnowcover=na.interpolation(data$cloudfreesnowcover,option="linear")

#Calculate the daily precipitation in cm/day
data$precip_daily=dissipate(data=data$precip_accum)
data$precip_daily=data$precip_daily*0.1

#Convert flow to cms if needed
data$flow=data$flow*0.0283

#May need to use na.interpolation from imputeTS package if daily values are missing
data$tmax=na.interpolation(data$tmax,option='linear')
data$tmin=na.interpolation(data$tmin,option='linear')
data$tobs=na.interpolation(data$tobs,option='linear')
data$tavg=na.interpolation(data$tavg,option='linear')

lagpad <- function(x, k) {
  c(rep(0, k), x)[1 : length(x)]
}

#Lag flow
data$lagflow=lagpad(data$flow,1)

#-------------------------------------------------------------------------------------------------------------------------------------------

ggplot(data)+geom_line(aes(x=date,y=flow))

model=lm(lagflow~tavg+albedo+cloudfreesnowcover+solar_short_Whm2day+precip_daily,data=data)
summary(model)
pr.flow=model$fitted.values
ggplot()+geom_point(aes(x=data$flow,y=pr.flow))
