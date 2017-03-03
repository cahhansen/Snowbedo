#' Calculates a modeled streamflow
#'
#' @param data A data frame with a shortwave radiation, albedo, average temperature, percent snowcover, and precipitation
#' @param meltcoefficient (melt factor*snow runoff coefficient*radiation coefficient)
#' @param runoffcoefficient (between 0-1)
#' @param area Area of watershed in Km2
#' @param baseflow Discharge in cms
#'
#' @return vector of modeled flow

modelflow=function(data,meltcoefficient,runoffcoefficient,area,baseflow){
  snowmelt=ifelse((data$tavg>0),((meltcoefficient*data$solar_short_Whm2day)*(1-data$albedo)*(data$tavg-0)),0)
  runoff = ifelse((data$tavg>0),runoffcoefficient*data$precip_daily,0)
  discharge=((snowmelt*data$snowcover)+runoff)*area*(10000/86400)+baseflow
  return(discharge)
}


