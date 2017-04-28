library(Snowbedo)
library(imputeTS)
library(caret)
library(ggplot2)
library(neuralnet)

#Read in data with all necessary parameters (and if needed, combine into a single data.frame)
data=read.csv('BigCottonwood.csv')

#Limit dataset to dates with common data
data=limitperiod(data=data,begin="2004-09-30",end="2011-10-01")

#-------------------------------------------------------------------------------------------------------------------------------------------
variables_list=c('flow','lagprecip_daily','tavg','tmax','tmin','albedo','solar_short_Whm2day','cloudfreesnowcover','snowdepth','swe')


#Create subset of parleys_data based on list of variables
sub_data=data[ , which(names(data) %in% variables_list)]


#Normalize the data
maxs = apply(sub_data,2,max)
mins = apply(sub_data,2,min)

norm_data = as.data.frame(scale(sub_data,center=mins, scale=maxs-mins))

index <- sample(1:nrow(norm_data),round(0.75*nrow(norm_data)))
train_ = norm_data[index,]
test_ = norm_data[-index,]

#Neural Network
set.seed(1)
n = names(train_)

#First create the formula which will be passed to the neuralnet fitting function
f=as.formula(paste("flow~",paste(n[!n %in% "flow"], collapse = "+")))

#Specify the number of neurons for each of the hidden layers using "hidden"
#Specify whether to do regression (linear.output=TRUE) or classification (linear.output=FALSE)
nn = neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

#Plot the nn structure
plot(nn)

#Predict using the nn
pr.nn = compute(nn,test_[,n[!n %in% "flow"]])
pr.nn_ = pr.nn$net.result*(max(sub_data$flow)-min(sub_data$flow))+min(sub_data$flow)
test.r = (test_$flow)*(max(sub_data$flow)-min(sub_data$flow))+min(sub_data$flow)

RMSE.nn = sqrt(sum((test.r - pr.nn_)^2)/nrow(test_))
print(paste("RMSE: ",RMSE.nn))

plot(test.r,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

testdates=data[-index,"date"]
plot(x=testdates,y=test.r,col=1,type="l",xlab="Date",ylab="Streamflow")
lines(x=testdates,y=pr.nn_,col=2)
