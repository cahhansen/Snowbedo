library(ggplot2)
library(Snowbedo)

#Read in data with all necessary parameters (and if needed, combine into a single data.frame)
data=read.csv('City.csv')

#Formatting---------------------------------------------------------------------------------------------------------
#Convert the shortwave radiation from W/m2/day to Wh/m2/day
data$solar_short_Whm2day=data$solar_short*24

#Convert snowcover to a percentage
data$snowcover=data$snowcover/100

#Limit dataset to dates with common data
completedata=limitperiod(data=data,begin="2000-03-01",end="2014-11-11")

#Calculate the daily precipitation in cm/day
completedata$precip_daily=dissipate(data=completedata$precip_accum)
completedata$precip_daily=completedata$precip_daily*0.1

#Convert flow to cms if needed
completedata$flow=completedata$flow*0.0283

#May need to use na.interpolation from imputeTS package if daily values are missing
#Parameters include: shortwave radiation, precipitation (daily or accumulative),
#percent of watershed covered in snow, average albedo in the watershed
#-------------------------------------------------------------------------------------------------------------------
#Set value for m (melt factor*snow runoff coefficient*radiation coefficient). This is basin-specific, and can be
#optimized by finding the m which results in the minimum error between modeled and observed streamflow
m=0.00001

#Set value for c (precipitation runoff coefficient)
c=0.4

#Define area of watershed in km2
area=45.1

#Baseflow (may be determined by the historical record)
b=0.1
#--------------------------------------------------------------------------------------------------------------------
#Streamflow model
completedata$predflow=modelflow(data=completedata,meltcoefficient=m,runoffcoefficient=c,area=area,baseflow=b)

#Set condition so that c=0 when precip is snow (depends on some threshold of average temperature)

ggplot(data=completedata)+
  geom_point(aes(x=date,y=flow,colour="Observed"))+
  geom_point(aes(x=date,y=predflow,colour="Modeled"))+
  scale_colour_manual("",
                      breaks = c("Observed","Modeled"),
                      values = c("#cc0000","#000099"))

summary(completedata$flow)
summary(completedata$predflow)


ggplot(data=completedata)+geom_point(aes(x=flow,y=predflow))
modelperform=lm(completedata$flow~completedata$predflow)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(modelperform$residuals)
