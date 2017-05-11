#CreateScenarios-------------------------------------------------------------------------------------------------
library(hydrostats)
#Load Data
watershed="Lambs"
load(paste0('Formatted/',watershed,".RData"))
load(paste0(watershed,'nn.rda'))
variables_list=c('Streamflow','Tmax_C','Tmin_C','Albedo','SnowCover','SnowDepth_cm','SolarRad_Whm2d','Precip_cm')

#Create subset of parleys_data based on list of variables
sub_data=formatteddata[ , which(names(formatteddata) %in% variables_list)]

#Normalize data
maxs = apply(sub_data,2,max)
mins = apply(sub_data,2,min)
norm_data = as.data.frame(scale(sub_data,center=mins, scale=maxs-mins))
n = names(norm_data)

#Apply NN model to baseline data
pr.streamflow=compute(nn,norm_data[,n[!n %in% "Streamflow"]])
pr.streamflow=pr.streamflow$net.result
#Transform from normalized values to "real" streamflow values
pr.streamflow = pr.streamflow*(max(formatteddata$Streamflow)-min(formatteddata$Streamflow))+min(formatteddata$Streamflow)
#Clean modeled results (limit streamflow to the baseflow)
  #Calculate baseflow
  #Calculate average annual minimum
flowts=ts.format(formatteddata, format="%Y-%m-%d", cols=c(1,2))
base=baseflows(flowts, n.reflected = 7, ts = "mean")
b=base$mean.bf
pr.streamflow[pr.streamflow<b]=b


AlbedoChange=c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
scenariostreamflow=data.frame(Date=formatteddata$Date,pr.streamflow)
for (i in AlbedoChange){
  ChangeinAlbedo=(1-i)*100
  scenarioname=paste0(ChangeinAlbedo,'_pr.streamflow')
#Decrease Albedo
modifieddata=sub_data
modifieddata$Albedo=modifieddata$Albedo*i
normmod_data = as.data.frame(scale(modifieddata,center=mins, scale=maxs-mins))
pr.streamflowmod=compute(nn,normmod_data[,n[!n %in% "Streamflow"]])
pr.streamflowmod=pr.streamflowmod$net.result
#Transform from normalized values to "real" streamflow values
pr.streamflowmod = pr.streamflowmod*(max(formatteddata$Streamflow)-min(formatteddata$Streamflow))+min(formatteddata$Streamflow)

#Clean modeled results (limit streamflow to the baseflow)
#Calculate baseflow
flowts=ts.format(scenariostreamflow, format="%Y-%m-%d", cols=c(1,2))
base=baseflows(flowts, n.reflected = 7, ts = "mean")
mb=base$mean.bf
pr.streamflowmod[pr.streamflowmod<b]=b
scenariostreamflow[scenarioname] <- pr.streamflowmod[,1]





plot(x=scenariostreamflow$Date,y=scenariostreamflow$pr.streamflow,type='l',lwd=3,col=1,main=paste0(ChangeinAlbedo,' Percent Decrease in Albedo'),
     xlab="Date",ylab="Streamflow, cms")
modelcolumn=scenariostreamflow[scenarioname]
lines(x=scenariostreamflow$Date,y=modelcolumn[,1],col=2)
legend("topright",c('Baseline','Scenario'),lty=c(1,1),lwd=c(1,1),col=c(1,2))

}

write.csv(scenariostreamflow,file=paste0(watershed,"AlbedoScenarios.csv"))
