#' Calculates daily precipitation from accumulative precipitation
#'
#' @param data A vector of accumulative precipitation data
#' @return A vector of daily precipitation
#'
#'



dissipate=function(data){
  len=length(data)
  precip_daily=rep(0,len)
  for (i in seq(1,len-1,by=1)){
    precip_daily[i+1]=data[i+1]-data[i]
  }
  #May need to reset at each water-year
  precip_daily=ifelse(precip_daily<0,0,precip_daily)
  return(precip_daily)
}

