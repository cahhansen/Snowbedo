library(reshape2)
library(ggplot2)
library(lubridate)
library(plyr)
#Set directory
setwd('C:/Users/carly/Google Drive/University of Utah - Research/Research Assistant/SnowpackDynamics (iUTAH and NASA projects)/ModelResults/Shifted/Exposure')
#List files in folder
filelist=list.files(path=".",pattern="*.csv")
numfiles=length(filelist)
scenarios=c("10 Percent Decrease in Albedo","20 Percent Decrease in Albedo","30 Percent Decrease in Albedo",
            "40 Percent Decrease in Albedo","50 Percent Decrease in Albedo","60 Percent Decrease in Albedo",
            "70 Percent Decrease in Albedo","80 Percent Decrease in Albedo","90 Percent Decrease in Albedo","Baseline")

exposuredf=data.frame(Year=0,Exp_BC=0,Exp_LC=0,Exp_PC=0,Exp_CC=0,Exp_System=0,Scenario=0)
for (i in seq(1,numfiles)){
  tempdata=read.csv(filelist[i])
  tempdata$Date=as.Date(strptime(tempdata$Date,format="%m/%d/%Y"))
  tempdata$Year=year(tempdata$Date)

  avgBC=aggregate(Exp_BC ~ Year, tempdata, mean)
  avgLC=aggregate(Exp_LC ~ Year, tempdata, mean)
  avgPC=aggregate(Exp_PC ~ Year, tempdata, mean)
  avgCC=aggregate(Exp_CC ~ Year, tempdata, mean)
  avgSystem=aggregate(Exp_System ~ Year, tempdata, mean)
  expdf=cbind(avgBC,avgLC,avgPC,avgCC,avgSystem)
  expdf=expdf[,-c(3,5,7,9)]
  expdf$Scenario=scenarios[i]

  exposuredf=rbind(exposuredf,expdf)

  png(file = paste0(scenarios[i],".png"))
  with(tempdata,plot(x=Date,y=Exp_BC,type="l",ylab="Exposure",xlab="Date",ylim=c(0,1),
                     main=paste0("Exposure \n in Wasatch Streams - ",scenarios[i])))
  with(tempdata,lines(x=Date,y=Exp_CC,col=2))
  with(tempdata,lines(x=Date,y=Exp_LC,col=3))
  with(tempdata,lines(x=Date,y=Exp_CC,col=4))
  dev.off()
}
exposuredf=exposuredf[-1,]
aggregate(. ~ Scenario, exposuredf, mean)



