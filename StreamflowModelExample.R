
data=read.csv('City.csv')

#Formatting
#Convert the shortwave radiation from W/m2/day to Wh/m2/day
data$solar_short_Whm2day=data$solar_short*24


#Limit dataset to dates with common data
completedata=limitperiod(data,begin="2000-03-01",end="2014-11-11")

#Calculate the daily precipitation

#Lag precip_accum by one day and find the difference
lagpad <- function(x, k) {
  c(rep(NA, k), x)[1 : length(x)]
}

data$temp=lagpad(data$precip_accum,1)
data$precip=data$precip_accum-data$temp
data=subset(data,select=-c(temp))

#Set value for m (melt factor*snow runoff coefficient*radiation coefficient). This is basin-specific, and can be
#optimized by finding the m which results in the minimum error between modeled and observed streamflow

m=0.001

#Set value for c (precipitation runoff coefficient)

c=0.5

#Define area of watershed in km2
area=100

#Streamflow model
streamflow=(((m*data$solar_short_Whm2day)*(1-data$albedo)*(data$tavg-0))*data$snowcover)+(c*data$precip)*area*(10000/86400)
#Set condition so that c=0 when precip is snow (depends on some threshold of average temperature)

plot(data$date,streamflow)
