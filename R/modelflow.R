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
  #Snowmelt process
  snowmelt=ifelse((data$Tavg_C>0),((meltcoefficient*data$SolarRad_Whm2d)*(1-data$Albedo)*(data$Tmax_C-0)),0)
  #Rainfall-runoff process
  runoff = ifelse((data$Tavg_C>0),runoffcoefficient*data$Precip_cm,0)
  #Discharge
  discharge=((snowmelt*data$SnowCover))*area*(10000/86400)+baseflow
  #Routed discharge
  return(list(snowmelt,runoff,discharge))
}


