library(ggplot2)
library(Snowbedo)
library(imputeTS)
library(lubridate)
library(hydrostats)
library(gridExtra)

#Read in data with all necessary parameters (and if needed, combine into a single data.frame)
#load('data/citycreekdata.RData')

data=read.csv('BigCottonwood.csv')

#Limit dataset to dates with common data
data=limitperiod(data=data,begin="2000-09-30",end="2011-10-01")

#Examine the autocorrelation
acf(data$flow)
pacf(data$flow)

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

#Parameters for model must include: shortwave radiation, precipitation (daily or accumulative),
#percent of watershed covered in snow, average albedo in the watershed
#-------------------------------------------------------------------------------------------------------------------
#Set value for m (melt factor*snow runoff coefficient*radiation coefficient). This is basin-specific, and can be
#optimized by finding the m which results in the minimum error between modeled and observed streamflow
m=0.00001

#Set value for c (precipitation runoff coefficient)
c=0.55

#Define area of watershed in km2
area=129.2

#Baseflow (may be determined by the historical record)
#Calculate average annual minimum
flowts=ts.format(data, format="%Y-%m-%d", cols=c(1,10))
base=baseflows(flowts, n.reflected = 30, ts = "mean")
b=base$mean.bf
#--------------------------------------------------------------------------------------------------------------------
#Streamflow model
modelresults=modelflow(data=data,meltcoefficient=m,runoffcoefficient=c,area=area,baseflow=b)
data$predflow=modelresults[[3]]
data$snowmelt=modelresults[[1]]
data$runoff=modelresults[[2]]
summary(data$flow)
summary(data$predflow)

plotobs=ggplot(data=data)+geom_point(aes(x=date,y=flow))+ggtitle("Observed Streamflow")
plotmodel=ggplot(data=data)+geom_point(aes(x=date,y=predflow))+ggtitle("Modeled Streamflow")
plotprecip=ggplot(data=data)+geom_point(aes(x=date,y=precip_accum))+ggtitle("Precipitation")
plotsnow=ggplot(data=data)+geom_point(aes(x=date,y=snowcover))+ggtitle("Snowcover")
plotsnowmelt=ggplot(data=data)+geom_point(aes(x=date,y=snowmelt))+ggtitle("Snowmelt")
grid.arrange(plotobs,plotmodel,ncol=1)
grid.arrange(plotobs,plotprecip,ncol=1)
grid.arrange(plotobs,plotsnow,ncol=1)
grid.arrange(plotmodel,plotsnow,ncol=1)
grid.arrange(plotobs,plotmodel,plotsnowmelt,plotsnow,plotprecip,ncol=1)

ggplot(data=data)+
  geom_point(aes(x=date,y=flow,colour="Observed"))+
  geom_point(aes(x=date,y=predflow,colour="Modeled"))+
  scale_colour_manual("",
                      breaks = c("Observed","Modeled"),
                      values = c("#cc0000","#000099"))
ggplot(data=data)+
  geom_point(aes(x=date,y=predflow,colour="Modeled"))+
  geom_point(aes(x=date,y=flow,colour="Observed"))+
  geom_point(aes(x=date,y=snowcover,colour="Snowcover"))+
  scale_colour_manual("",
                      breaks = c("Snowcover","Modeled","Observed"),
                      values = c("#cc0000","#000000","#000099"))

modelperform=lm(data$flow~data$predflow)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(modelperform$residuals)

#Look at peaks
data$Year=year(data$date)
data$DOY=yday(data$date)
peaks=data.frame(Year=seq(min(data$Year)+1,max(data$Year)))
for (i in seq(min(data$Year)+1,max(data$Year))){
  streamsub=na.omit(data[(data$Year==i),])
  peak=streamsub[(streamsub$flow==max(streamsub$flow)),"DOY"]
  peakmod=streamsub[(streamsub$predflow==max(streamsub$predflow)),"DOY"]
  peaks[(i-(min(data$Year))),"obs"]=peak
  peaks[(i-(min(data$Year))),"mod"]=peakmod
}
peaks$Diff=peaks$obs-peaks$mod
avgdiff=mean(peaks$Diff)
summary(peaks$Diff)


#-------------------------------------------------------------------------------------------------------

