library(randomForest)
library(ggplot2)
library(Snowbedo)
library(caret)
library(gridExtra)

#Read in data with all necessary parameters (and if needed, combine into a single data.frame)
data=load()

#Limit dataset to dates with common data
data=limitperiod(data=data,begin="2004-09-30",end="2011-10-01")

#Define Variables of Interest
variables_list=c('flow','precip_daily','tavg','albedo','solar_short_Whm2day','cloudfreesnowcover','lagprecip_daily','snowdepth','swe')


#Create subset of parleys_data based on list of variables
watershed_data=data[ , which(names(data) %in% variables_list)]

#Random Forest
set.seed(1)
fitFull.rf <- train(flow~., data=watershed_data, method="rf")
rf.results=fitFull.rf$finalModel
rf.predflow=rf.results$predicted
data$predflow=rf.predflow

plotobs=ggplot(data=data)+geom_line(aes(x=date,y=flow))+ggtitle("Observed Streamflow")
plotmodel=ggplot(data=data)+geom_line(aes(x=date,y=predflow))+ggtitle("Modeled Streamflow")
grid.arrange(plotobs,plotmodel,ncol=1)
summary(data$flow)
summary(data$predflow)
#Compute RMSE:
rmse=sqrt(mean((data$predflow-data$flow)^2))
print(paste0('RMSE:',rmse))

plot(x=data$date,y=data$flow,col=1,type="l",xlab="Date",ylab="Streamflow")
lines(x=data$date,y=data$predflow,col=2)
