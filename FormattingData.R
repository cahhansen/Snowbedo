library(imputeTS)

data=read.csv('BigCottonwood.csv')

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
data$snowdepth=na.interpolation(data$snowdepth,option='linear')

lagpad <- function(x, k) {
  c(rep(0, k), x)[1 : length(x)]
}

#Lag predictive variables
data$lagprecip_daily=lagpad(data$precip_daily,1)
data$lagtmax=lagpad(data$tmax,1)
data$lagtmin=lagpad(data$tmin,1)
data$lagtavg=lagpad(data$tavg,1)
data$lagswe=lagpad(data$swe,1)
data$lagsolar=lagpad(data$solar_short_Whm2day,1)
data$lagalbedo=lagpad(data$albedo,1)
data$lagsnowcover=lagpad(data$cloudfreesnowcover,1)
data$lagsnowdepth=lagpad(data$snowdepth,1)

#Save data
