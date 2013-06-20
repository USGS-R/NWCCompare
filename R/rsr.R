#' Function to return the ratio of the root mean square error to the standard deviation
#' 
#' This function accepts observed and modeled daily data series and returns the root mean square error/standard deviation
#' 
#' @param timeseries1 data frame containing value data for the observed timeseries
#' @param timeseries2 data frame containing value data for the modeled timeseries
#' @return rsr root mean square error/standard deviation for the two timeseries
#' @export
#' @examples
#' obs_data<-paste(system.file(package="NWCCompare"),"/data/obs_data.csv",sep="")
#' mod_data<-paste(system.file(package="NWCCompare"),"/data/mod_data.csv",sep="")
#' timeseries1<-read.csv(obs_data)$discharge
#' timeseries2<-read.csv(mod_data)$discharge
#' rsr(timeseries1,timeseries2)
rsr<-function(timeseries1,timeseries2) {
  sqerror<-(timeseries1-timeseries2)^2
  sumsqerr<-sum(sqerror)
  n<-length(timeseries1)
  rmse<-sqrt(sumsqerr/n)
  sdev <- sd(timeseries1,na.rm=TRUE)
  rsr <- rmse/sdev
  return(rsr)
}