#' Limits data to specified time period
#'
#' @param data A data frame with a date column
#' @param begin A string, Start of desired time period in the format YYYY-mm-dd
#' @param end A string, End of desired time period in the format YYYY-mm-dd
#' @return data.frame that is limited to the time period between begin and end
#'

limitperiod = function(data,begin,end){
  data$date=as.Date(data$date,format="%m/%d/%Y")
  data=data[(data$date>=begin & data$date<=end),]
  return(data)
}
