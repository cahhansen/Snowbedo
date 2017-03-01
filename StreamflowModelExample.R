library(ggplot2)
library(Snowbedo)

#Read in data with all necessary parameters (and if needed, combine into a single data.frame)
data=read.csv('City.csv')

#Formatting
#Convert the shortwave radiation from W/m2/day to Wh/m2/day
data$solar_short_Whm2day=data$solar_short*24

#Limit dataset to dates with common data
completedata=limitperiod(data=data,begin="2000-03-01",end="2014-11-11")

#Calculate the daily precipitation
completedata$precip_daily=dissipate(data=completedata$precip_accum)

#Set value for m (melt factor*snow runoff coefficient*radiation coefficient). This is basin-specific, and can be
#optimized by finding the m which results in the minimum error between modeled and observed streamflow

m=0.001

#Set value for c (precipitation runoff coefficient)

c=0.5

#Define area of watershed in km2
area=100

#Streamflow model

#Set condition so that c=0 when precip is snow (depends on some threshold of average temperature)

ggplot(data=completedata)+
  geom_point(aes(x=date,y=flow,colour="Observed"))+
  geom_point(aes(x=date,y=predflow,colour="Modeled"))+
  scale_colour_manual("",
                      breaks = c("Observed","Modeled"),
                      values = c("#cc0000","#000099"))
