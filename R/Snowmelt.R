path='changethis'
setwd(path)
data=read.csv('City.csv')

#Formatting
#Convert the shortwave radiation from W/m2/day to Wh/m2/day
data$solar_short_Whm2day=data$solar_short*24
#Calculate the daily precipitation
#Lag precip_accum by one day and find the difference
data$precip=data$precip_accum

#Set value for m (melt factor*snow runoff coefficient*radiation coefficient)



#Snowmelt model
snowmelt=(m*data$solar_short_Whm2day)*(1-data$albedo)*(data$tavg-0)

