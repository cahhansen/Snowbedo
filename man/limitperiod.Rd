% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/limitperiod.R
\name{limitperiod}
\alias{limitperiod}
\title{Limits data to specified time period}
\usage{
limitperiod(data, begin, end)
}
\arguments{
\item{data}{A data frame with a date column}

\item{begin}{A string, Start of desired time period in the format YYYY-mm-dd}

\item{end}{A string, End of desired time period in the format YYYY-mm-dd}
}
\value{
data.frame that is limited to the time period between begin and end
}
\description{
Limits data to specified time period
}
