library(imputeTS)
library(Snowbedo)
library(lubridate)


watershed="LittleCottonwood"
data=read.csv(paste0(watershed,'.csv'))

#Formatting---------------------------------------------------------------------------------------------------------
data$date=as.Date(data$date,format="%m/%d/%Y")
#Convert the shortwave radiation from W/m2/day to Wh/m2/day
data$solar_short_Whm2day=data$solar_short*24

#Convert snowcover to a percentage (0-1)
data$snowcover=data$snowcover/100
#Interpolate snowcover data for "cloud free" conditions
data$cloudfreesnowcover=data$snowcover
data[(data$cloudcover>25 | is.na(data$cloudcover)),"cloudfreesnowcover"]=NA
data$cloudfreesnowcover=na.interpolation(data$cloudfreesnowcover,option="linear")

#Calculate the daily precipitation in cm/day
data$precip_daily=dissipate(data=data$precip_accum)


#Convert flow to cms if needed
data$flow=data$flow*0.0283

#Convert swe and snowdepth to cm
data$swe=data$swe*2.54
data$snowdepth=data$snowdepth*2.54

#May need to use na.interpolation from imputeTS package if daily values are missing
data$flow=na.interpolation(data$flow,option='linear')
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

formatteddata=data[,c("date","flow","tmax","tmin","tavg","swe","albedo","solar_short_Whm2day","cloudfreesnowcover","snowdepth","precip_daily",
                      "lagtmax","lagtmin","lagtavg","lagswe","lagalbedo","lagsolar","lagsnowcover","lagsnowdepth","lagprecip_daily")]

colnames(formatteddata) <- c("Date", "Streamflow","Tmax_C","Tmin_C","Tavg_C","SWE_cm","Albedo","SolarRad_Whm2d","SnowCover",
                        "SnowDepth_cm","Precip_cm","LagTmax","LagTmin","LagTavg","LagSWE","LagAlbedo","LagSolarRad","LagSnowCover",
                        "LagSnowDepth","LagPrecip")

#Limit dataset to dates with common data
formatteddata=limitperiod(data=formatteddata,begin="2004-10-01",end="2011-09-30")
save(formatteddata,file=paste0("C:/Users/Carly/Desktop/SnowpackModeling/Snowbedo/data/Formatted/",watershed,".RData"))
formatteddata[is.na(formatteddata)] <- ""
formatteddata$Month=month(formatteddata$Date)
write.csv(formatteddata,paste0("C:/Users/Carly/Desktop/SnowpackModeling/",watershed,".csv"))

