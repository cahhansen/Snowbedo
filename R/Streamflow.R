path='changethis'
setwd(path)
data=read.csv('City.csv')
snowmelt=modeledsnowmelt

#Set value for c (precipitation runoff coefficient)

#Define area of watershed in km2
area=100

#Streamflow model
streamflow=(snowmelt*data$snowcover)+(c*data$precip)*area*(10000/86400)
#Set condition so that c=0 when precip is snow (depends on some threshold of average temperature)
