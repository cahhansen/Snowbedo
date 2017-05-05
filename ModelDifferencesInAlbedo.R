#CreateScenarios-------------------------------------------------------------------------------------------------
library(hydrostats)
#Load Data
watershed="BigCottonwood"
load(paste0('Formatted/',watershed,".RData"))
load(paste0(watershed,'nn2.rda'))
variables_list=c('Streamflow','Tmax_C','Albedo','SnowDepth_cm','SolarRad_Whm2d','Precip_cm')

#Create subset of parleys_data based on list of variables
sub_data=formatteddata[ , which(names(formatteddata) %in% variables_list)]

#Normalize data
maxs = apply(sub_data,2,max)
mins = apply(sub_data,2,min)
norm_data = as.data.frame(scale(sub_data,center=mins, scale=maxs-mins))
n = names(norm_data)

#Apply NN model to baseline data
pr.streamflow=compute(nn,norm_data[,n[!n %in% "Streamflow"]])
#Transform from normalized values to "real" streamflow values
pr.streamflow = pr.streamflow$net.result*(max(formatteddata$Streamflow)-min(formatteddata$Streamflow))+min(formatteddata$Streamflow)
#Clean modeled results (limit streamflow to the baseflow)
  #Calculate baseflow
  #Calculate average annual minimum
modeledstreamflow=data.frame(Date=formatteddata$Date,pr.streamflow)
flowts=ts.format(modeledstreamflow, format="%Y-%m-%d", cols=c(1,2))
base=baseflows(flowts, n.reflected = 7, ts = "mean")
b=base$mean.bf
modeledstreamflow[modeledstreamflow$pr.streamflow<b,"pr.streamflow"]=b


AlbedoChange=c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)

for (i in AlbedoChange){
#Decrease Albedo
modifieddata=sub_data
modifieddata$Albedo=modifieddata$Albedo*i
maxs = apply(modifieddata,2,max)
mins = apply(modifieddata,2,min)
normmod_data = as.data.frame(scale(modifieddata,center=mins, scale=maxs-mins))
pr.streamflowmod=compute(nn,normmod_data[,n[!n %in% "Streamflow"]])
#Transform from normalized values to "real" streamflow values
pr.streamflowmod = pr.streamflowmod$net.result*(max(formatteddata$Streamflow)-min(formatteddata$Streamflow))+min(formatteddata$Streamflow)
#Clean modeled results (limit streamflow to the baseflow)
#Calculate baseflow
scenariostreamflow=data.frame(Date=formatteddata$Date,pr.streamflowmod)
flowts=ts.format(scenariostreamflow, format="%Y-%m-%d", cols=c(1,2))
base=baseflows(flowts, n.reflected = 7, ts = "mean")
mb=base$mean.bf
scenariostreamflow[scenariostreamflow$pr.streamflowmod<mb,"pr.streamflowmod"]=b



ChangeinAlbedo=1-i

plot(modeledstreamflow$Date,modeledstreamflow$pr.streamflow,type='l',lwd=3,col=1,main=paste0(ChangeinAlbedo,' Decrease in Albedo'))
lines(scenariostreamflow$Date,scenariostreamflow$pr.streamflowmod,col=2)
legend("topright",c('Baseline','Scenario'),lty=c(1,1),lwd=c(1,1),col=c(1,2))

}

