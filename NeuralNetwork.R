library(Snowbedo)
library(imputeTS)
library(caret)
library(ggplot2)
library(neuralnet)

watershed='BigCottonwood'

#Read in data with all necessary parameters (and if needed, combine into a single data.frame)
load(paste0('Formatted/',watershed,'.RData'))
data=formatteddata

#-------------------------------------------------------------------------------------------------------------------------------------------
variables_list=c('Streamflow','Tmax_C','Tmin_C','Albedo','SolarRad_Whm2d','SnowCover','SnowDepth_cm','Precip_cm')
#variables_list=c('Streamflow','Tmax_C','Albedo','SnowDepth_cm','SolarRad_Whm2d','Precip_cm')

#Create subset of parleys_data based on list of variables
sub_data=data[ , which(names(data) %in% variables_list)]



#Normalize the data
maxs = apply(sub_data,2,max)
mins = apply(sub_data,2,min)
norm_data = as.data.frame(scale(sub_data,center=mins, scale=maxs-mins))

#Create training and testing datasets
set.seed(1)
index <- sample(1:nrow(norm_data),round(0.75*nrow(norm_data)))
train_ = norm_data[index,]
test_ = norm_data[-index,]
#Neural Network
n = names(train_)
#First create the formula which will be passed to the neuralnet fitting function
f=as.formula(paste("Streamflow~",paste(n[!n %in% "Streamflow"], collapse = "+")))
#Specify the number of neurons for each of the hidden layers using "hidden"
#Specify whether to do regression (linear.output=TRUE) or classification (linear.output=FALSE)
nn = neuralnet(f,data=train_,hidden=c(2),linear.output=T,threshold=0.05)


#Plot the nn structure
plot(nn)

#Predict using the nn
pr_train=compute(nn,train_[,n[!n %in% "Streamflow"]])
pr_test = compute(nn,test_[,n[!n %in% "Streamflow"]])
#Transform from normalized values to "real" streamflow values
pr_train = pr_train$net.result*(max(sub_data$Streamflow)-min(sub_data$Streamflow))+min(sub_data$Streamflow)
pr_test = pr_test$net.result*(max(sub_data$Streamflow)-min(sub_data$Streamflow))+min(sub_data$Streamflow)
train.r = (train_$Streamflow)*(max(sub_data$Streamflow)-min(sub_data$Streamflow))+min(sub_data$Streamflow)
test.r = (test_$Streamflow)*(max(sub_data$Streamflow)-min(sub_data$Streamflow))+min(sub_data$Streamflow)

RMSE.train = sqrt(sum((train.r - pr_train)^2)/nrow(train_))
print(paste("RMSE: ",RMSE.train))
RMSE.test = sqrt(sum((test.r - pr_test)^2)/nrow(test_))
print(paste("RMSE: ",RMSE.test))

traindates=data[index,"Date"]
testdates=data[-index,"Date"]
comparisondates=append(traindates,testdates)
comparisonvalues=append(pr_train,pr_test)
actualvalues=append(train.r,test.r)
comparisondf=data.frame(Date=comparisondates,Predicted=comparisonvalues,Actual=actualvalues)
comparisondf=comparisondf[order(comparisondf$Date),]


#with(comparisondf,plot(Actual,Predicted,col='red',main='Actual vs Predicted Streamflow',pch=18,cex=0.7))
#abline(0,1,lwd=2)

with(comparisondf,plot(x=Date,y=Actual,col=1,type="l",xlab="Date",ylab="Streamflow",ylim=c(0,max(Actual)*1.25)))
with(comparisondf,lines(x=Date,y=Predicted,col=2))
legend("topright",c('Actual','Predicted'),lty=c(1,1),lwd=c(1,1),col=c(1,2))

#Save the NN model (can be used to model future scenarios)
save(nn, file = paste0(watershed,"nn2.rda"))


