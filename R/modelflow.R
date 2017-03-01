#' Calculates a modeled streamflow
#'
#' @param data A data frame with a shortwave radiation, albedo, average temperature, percent snowcover, and precipitation
#' @param meltcoefficient (melt factor*snow runoff coefficient*radiation coefficient)
#' @param runoffcoefficient
#' @param area of watershed in Km2
#' @return vector of modeled flow
#'

modelflow=function(data,meltcoefficient,runoffcoefficient,area){
  flow=(((meltcoefficient*data$solar_short_Whm2day)*(1-data$albedo)*
      (data$tavg-0))*data$snowcover)+(runoffcoefficient*data$precip)*area*(10000/86400)
  return(flow)
}
