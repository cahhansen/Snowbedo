library(reshape2)
library(ggplot2)
library(lubridate)
library(plyr)
#Set directory
setwd('C:/Users/carly/Google Drive/University of Utah - Research/Research Assistant/SnowpackDynamics (iUTAH and NASA projects)/ModelResults/SupplySources')
#List files in folder
filelist=list.files(path=".",pattern="*.csv")
numfiles=length(filelist)
#Read in .csv files and format
supplydf=data.frame(Date=seq(as.Date("2004-10-01"), as.Date("2011-9-30"), by="days"))
supplydf$Year=year(supplydf$Date)
scenarios=c("10 Percent Decrease in Albedo","20 Percent Decrease in Albedo","30 Percent Decrease in Albedo",
            "40 Percent Decrease in Albedo","50 Percent Decrease in Albedo","60 Percent Decrease in Albedo",
            "70 Percent Decrease in Albedo","80 Percent Decrease in Albedo","90 Percent Decrease in Albedo","Baseline")
for (i in seq(1,numfiles)){
  tempdata=read.csv(filelist[i])
  tempdata$Date=as.Date(strptime(tempdata$Date,format="%m/%d/%Y"))
  #Put in single dataframe
  supplydf[,i+2]=tempdata[,3]

  png(file = paste0(scenarios[i],".png"))
  with(tempdata,plot(x=Date,y=ExchangeSources,ylab="Acre-Feet",xlab="Date",ylim=c(0,120),
                     main=paste0("Amount of Exchange Water Supply \n Used by SLC - ",scenarios[i])))
  dev.off()
}
names(supplydf)=c("Date","Year","Exchange_10","Exchange_20","Exchange_30","Exchange_40","Exchange_50",
                  "Exchange_60","Exchange_70","Exchange_80","Exchange_90","Baseline")


#Summarize Use of Exchange Water by Year
sumdf=aggregate(. ~ Year, supplydf, sum)
sumdf=sumdf[,-2]
countsdf=aggregate(.~ Year, supplydf, FUN=function(x) sum((x!=0)))
countsdf=countsdf[,-2]


