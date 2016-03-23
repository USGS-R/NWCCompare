#' Function to return the 10 year return value for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the 10 year return value, 
#' calculated for the National Water Census by a non-standard method.
#' 
#' @param qfiletempf data frame containing value data for the chosen timeseries
#' @return NWCportalReturn10 10-year return value for the given data frame
#' @export
#' @examples
#' qfiletempf<-sampleData
#' NWCportalReturn10(qfiletempf)
NWCportalReturn10 <- function(qfiletempf) {
  annual_max <- aggregate(qfiletempf$discharge, list(qfiletempf$wy_val), max, na.rm=TRUE)
  sort_annual_max <- sort(annual_max$x)
  rank_10 <- floor(findrank(length(sort_annual_max),0.10))
  return_10 <- sort_annual_max[rank_10]
  return(return_10)
}