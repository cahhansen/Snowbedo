library(ggplot2)
library(Snowbedo)
library(imputeTS)
library(lubridate)
library(hydrostats)
library(gridExtra)

#Read in data with all necessary parameters (and if needed, combine into a single data.frame)

watershed='BigCottonwood'
load(paste0('Formatted/',watershed,'.RData'))

#Parameters for model must include: shortwave radiation, precipitation (daily or accumulative),
#percent of watershed covered in snow, average albedo in the watershed
#-------------------------------------------------------------------------------------------------------------------
#Set value for m (melt factor*snow runoff coefficient*radiation coefficient). This is basin-specific, and can be
#optimized by finding the m which results in the minimum error between modeled and observed streamflow
m=0.000012

#Set value for c (precipitation runoff coefficient)
c=0.50

#Define area of watershed in km2
area=129.2

#Baseflow (may be determined by the historical record)
#Calculate average annual minimum
flowts=ts.format(formatteddata, format="%Y-%m-%d", cols=c(1,2))
base=baseflows(flowts, n.reflected = 7, ts = "mean")
b=base$mean.bf
#--------------------------------------------------------------------------------------------------------------------
#Streamflow model
modelresults=modelflow(data=formatteddata,meltcoefficient=m,runoffcoefficient=c,area=area,baseflow=b)
modeleddata=data.frame(Date=formatteddata$Date,Streamflow=formatteddata$Streamflow,predflow=modelresults[[3]],snowmelt=modelresults[[1]],runoff=modelresults[[2]])
summary(formatteddata$Streamflow)
summary(modeleddata$predflow)

plotobs=ggplot(data=formatteddata)+geom_point(aes(x=Date,y=Streamflow))+ggtitle("Observed Streamflow")
plotmodel=ggplot(data=modeleddata)+geom_point(aes(x=Date,y=predflow))+ggtitle("Modeled Streamflow")
grid.arrange(plotobs,plotmodel,ncol=1)


ggplot(data=modeleddata)+
  geom_point(aes(x=Date,y=Streamflow,colour="Observed"))+
  geom_point(aes(x=Date,y=predflow,colour="Modeled"))+
  scale_colour_manual("",
                      breaks = c("Observed","Modeled"),
                      values = c("#cc0000","#000099"))

modelperform=lm(modeleddata$Streamflow~modeleddata$predflow)
rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(modelperform$residuals)

#Look at peaks
modeleddata$Year=year(modeleddata$Date)
modeleddata$DOY=yday(modeleddata$Date)
peaks=data.frame(Year=seq(min(modeleddata$Year)+1,max(modeleddata$Year)))
for (i in seq(min(data$Year)+1,max(data$Year))){
  streamsub=na.omit(modeleddata[(modeleddata$Year==i),])
  peak=streamsub[(streamsub$Streamflow==max(streamsub$Streamflow)),"DOY"]
  peakmod=streamsub[(streamsub$predflow==max(streamsub$predflow)),"DOY"]
  peaks[(i-(min(modeleddata$Year))),"obs"]=peak
  peaks[(i-(min(modeleddata$Year))),"mod"]=peakmod
}
peaks$Diff=peaks$obs-peaks$mod
avgdiff=mean(peaks$Diff)
summary(peaks$Diff)


#-------------------------------------------------------------------------------------------------------

