library(plyr)
library(lubridate)

#Population Data
popdata=read.csv('C:/Users/Carly/Google Drive/University of Utah - Research/Dissertation Research/SystemsModel/Goldsim Documentation/Data/PopulationData.csv')
popdata=na.omit(popdata)

#Streamflow Data
setwd("C:/Users/Carly/Desktop/SnowpackModeling/Snowbedo/data/Formatted")
load('BigCottonwood.RData')
watersheddata=formatteddata
watersheddata=watersheddata[(watersheddata$Date>="2004-01-01" & watersheddata$Date<="2013-12-31"),]

watersheddata$month=months(watersheddata$Date)
watersheddata$year=year(watersheddata$Date)

summaryalbedowatersheddata=as.matrix(with(watersheddata, tapply(Albedo, list("Month"=month, "Year"=year), mean)))
summaryalbedowatersheddata=as.data.frame(t(summaryalbedowatersheddata))
summaryalbedowatersheddata$year=rownames(summaryalbedowatersheddata)

summaryflowwatersheddata=as.matrix(with(watersheddata, tapply(Streamflow, list("Month"=month, "Year"=year), mean)))
summaryflowwatersheddata=as.data.frame(t(summaryflowwatersheddata))
summaryflowwatersheddata$year=rownames(summaryflowwatersheddata)


#Plot of Albedo vs Streamflow
for (i in c("March","April","May","June","July")){
par(mfrow=c(2,2),oma=c(0,0,2,0))


par(mar = c(5,5,2,5))
plot(x=summaryalbedowatersheddata$year,y=summaryalbedowatersheddata$January,xlab="Year",ylab="Avg. Albedo",main="January",ylim=c(0.1,0.5),type="l")
par(new = T)
with(summaryflowwatersheddata, plot(summaryflowwatersheddata[,i],type="l",col="blue", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Avg. Streamflow, cms')

par(mar = c(5,5,2,5))
plot(x=summaryalbedowatersheddata$year,y=summaryalbedowatersheddata$February,xlab="Year",ylab="Avg. Albedo",main="February",ylim=c(0.1,0.5),type="l")
par(new = T)
with(summaryflowwatersheddata, plot(summaryflowwatersheddata[,i],type="l",col="blue", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Avg. Streamflow, cms')

par(mar = c(5,5,2,5))
plot(x=summaryalbedowatersheddata$year,y=summaryalbedowatersheddata$March,xlab="Year",ylab="Avg. Albedo",main="March",ylim=c(0.1,0.5),type="l")
par(new = T)
with(summaryflowwatersheddata, plot(summaryflowwatersheddata[,i],type="l",col="blue", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Avg. Streamflow, cms')

par(mar = c(5,5,2,5))
plot(x=summaryalbedowatersheddata$year,y=summaryalbedowatersheddata$April,xlab="Year",ylab="Avg. Albedo",main="April",ylim=c(0.1,0.5),type="l")
par(new = T)
with(summaryflowwatersheddata, plot(summaryflowwatersheddata[,i],type="l",col="blue", axes=F, xlab=NA, ylab=NA, cex=1.2))
axis(side = 4)
mtext(side = 4, line = 3, 'Avg. Streamflow, cms')

title(paste0(i," Streamflow"), outer=TRUE)
}

plot(x=popdata$Year,y=popdata$SLCDPU,xlab="Year",ylab="Population",type="l",col="blue",main="Population within Bounds of Wasatch Stream Supplies")

colnames(summarywatersheddata)
