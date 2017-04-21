#' Function to return the rank of a given value
#' 
#' This function accepts the number of values in a data set and the desired percentile and then calculates the rank 
#' 
#' @param n number of values in a data set
#' @param p percentile desired
#' @return findrank numeric giving the position in the data set of the percentile
#' @examples
#' n<-365
#' p<-0.05
#' findrank(n, p)
findrank <- function(n, p) {
  r <- (1 - p) * (n + 1)
  findrank <- floor(r)
  return(findrank)
}

#' Function to return the requested flow percentiles for a given data series
#' 
#' This function accepts a data frame containing daily data and a list of desired percentiles and 
#' returns a list of the requested percentiles
#' 
#' @param flow_data A dataframe containing a NWCCompare flow dataset. 
#' Should have been cleaned by \link[EflowStats]{dataCheck}
#' @param probs vector containing requested percentile value(s)
#' @return obs_percentiles requested flow percentiles for the given data frame
#' @importFrom stats quantile
#' @examples
#' flow_data<-obs_data
#' flow_data$date <- as.Date(flow_data$date)
#' flow_data <- dataCheck(flow_data, yearType = "water")
#' flow_perc(flow_data, probs=c(.1,.25,.5,.75))
flow_perc <- function(flow_data, probs=c(.1,.25,.5,.75,.9,.15)) {
  obs_percentiles <- quantile(flow_data$discharge,probs,na.rm=TRUE)
  return(obs_percentiles)
}

#' Function to return the Nash-Sutcliffe value between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the Nash-Sutcliffe value
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return nse Nash-Sutcliffe value between the two timeseries
#' @examples
#' obs_data<-obs_data
#' mod_data<-mod_data
#' nse(obs_data$discharge,mod_data$discharge)
nse<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
    numerat<-sum((timeseries1-timeseries2)^2,na.rm=TRUE)
    denomin<-sum((timeseries1-mean(timeseries1,na.rm=TRUE))^2,na.rm=TRUE)  #6/18/11: NSE value calculation has been fixed
    nse<-(1-(numerat/denomin))
  } else {nse<-NA} 
  return(nse)
}

#' Function to return the Nash-Sutcliffe value between the natural logarithms of the two data series
#' 
#' This function accepts two data frames containing daily data series and returns the Nash-Sutcliffe value of the natural 
#' logarithms of the data, with zeros removed.
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return nselog Nash-Sutcliffe value between the natural log of the two timeseries
#' @examples
#' obs_data<-obs_data
#' mod_data<-mod_data
#' nselog(obs_data$discharge,mod_data$discharge)
nselog<-function(timeseries1,timeseries2) {
  # Count of zeros in dataset
  sszeros<-subset(timeseries1,timeseries1==0)
  czeros<-length(sszeros)
  
  # Put timeseries1 and timeseries2 into a data frame  and add header
  obsestq<-data.frame(timeseries1,timeseries2)
  colnames(obsestq)<-c("obs","est")
  #attach(obsestq)
  
  # If zeroes in timeseries1, display message and delete zeroes
  if (czeros>0) {
    cat("\n", czeros, "streamflows with a zero value were detected in this dataset. \nThese values will be removed before computing the \nNash-Sutcliffe efficiency value from the natural logs \nof the streamflows.")
  } else {} #Do nothing if no zeros
  nozeros<-subset(obsestq,obsestq$obs>0)
  
  if (nrow(nozeros)>1) {
    
    # Compute NS
    numerat<-sum((log(nozeros$obs)-log(nozeros$est))^2,na.rm=TRUE)
    denomin<-sum((log(nozeros$obs)-mean(log(nozeros$obs),na.rm=TRUE))^2,na.rm=TRUE)
    nselog<-(1-(numerat/denomin))
  } else {nselog<-NA}
  return(nselog)
}

#' Function to return the normalized root mean square error between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the normalized root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return rmsne normalized root mean square error value between the two timeseries
#' @examples
#' obs_data<-obs_data
#' mod_data<-mod_data
#' rmsne(obs_data$discharge,mod_data$discharge)
rmsne<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
    sqerror<-((timeseries1-timeseries2)/timeseries1)^2
    sumsqerr<-sum(sqerror)
    n<-length(timeseries1)
    rmsne<-sqrt(sumsqerr/n)
  } else {rmsne<-NA}
  return(rmsne)
}

#' Function to return the standard deviation for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the standard deviation
#' 
#' @param x data frame containing value data for the chosen timeseries
#' @return sdev standard deviation for the given data frame
#' @examples
#' obs_data<-obs_data
#' sdev(obs_data$discharge)
sdev <- function(x) {
  sdev <- sd(x,na.rm=TRUE)
  return(sdev)
}