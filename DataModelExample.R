library(randomForest)
library(ggplot2)

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


#-------------------------------------------------------------------------------------------------------------------------------------------
variables_list=c('flow','precip_daily','tavg','albedo','solar_short_Whm2day','cloudfreesnowcover')


#Create subset of parleys_data based on list of variables
watershed_data=data[ , which(names(data) %in% variables_list)]

#Random Forest
set.seed(1)
fitFull.rf <- train(flow~., data=watershed_data, method="rf")
rf.results=fitFull.rf$finalModel
rf.predflow=rf.results$predicted
data$predflow=rf.predflow

plotobs=ggplot(data=data)+geom_point(aes(x=date,y=flow))+ggtitle("Observed Streamflow")
plotmodel=ggplot(data=data)+geom_point(aes(x=date,y=predflow))+ggtitle("Modeled Streamflow")
grid.arrange(plotobs,plotmodel,ncol=1)
summary(data$flow)
summary(data$predflow)



#SVM (Support Vector Machine)
set.seed(1)
fitFull.svmRadial <- train(flow~., data=watershed_data, method="svmRadial")
