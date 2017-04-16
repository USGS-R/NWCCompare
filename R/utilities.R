#' Function to return the coefficient of variation for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the coefficient of variation
#' 
#' @param x data frame containing value data for the chosen timeseries
#' @return cv coefficient of variation for the given data frame
#' @examples
#' qfiletempf<-sampleData
#' cv(qfiletempf$discharge)
cv <- function(x) {
  x1 <- mean(x,na.rm=TRUE)
  x2 <- sdev(x)
  cv <- x2/x1
  return(cv)
}

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
#' @param data data frame containing value data for the chosen timeseries
#' @param probs vector containing requested percentile value(s)
#' @return obs_percentiles requested flow percentiles for the given data frame
#' @importFrom stats quantile
#' @examples
#' qfiletempf<-sampleData
#' flow_perc(qfiletempf,probs=c(.1,.25,.5,.75))
flow_perc <- function(data,probs=c(.1,.25,.5,.75,.9,.15)) {
  obs_percentiles <- quantile(data$discharge,probs,na.rm=TRUE)
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
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' nse(timeseries1,timeseries2)
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
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' nselog(timeseries1,timeseries2)
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

#' Function to return the root mean square error between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return rmse root mean square error value between the two timeseries
#' @examples
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' rmse(timeseries1,timeseries2)
rmse<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
    sqerror<-(timeseries1-timeseries2)^2
    sumsqerr<-sum(sqerror)
    n<-length(timeseries1)
    rmse<-sqrt(sumsqerr/n)
  } else {rmse<-NA}
  return(rmse)
}

#' Function to return the normalized root mean square error between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the normalized root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return rmsne normalized root mean square error value between the two timeseries
#' @examples
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' rmsne(timeseries1,timeseries2)
rmsne<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
    sqerror<-((timeseries1-timeseries2)/timeseries1)^2
    sumsqerr<-sum(sqerror)
    n<-length(timeseries1)
    rmsne<-sqrt(sumsqerr/n)
  } else {rmsne<-NA}
  return(rmsne)
}

#' Function to return the ratio of the root mean square error to the standard deviation
#' 
#' This function accepts observed and modeled daily data series and returns the root mean square error/standard deviation
#' 
#' @param timeseries1 data frame containing value data for the observed timeseries
#' @param timeseries2 data frame containing value data for the modeled timeseries
#' @return rsr root mean square error/standard deviation for the two timeseries
#' @examples
#' obs_data<-dailyData
#' mod_data<-dailyData
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' rsr(timeseries1,timeseries2)
rsr<-function(timeseries1,timeseries2) {
  if (length(timeseries1)>1) {
    sqerror<-(timeseries1-timeseries2)^2
    sumsqerr<-sum(sqerror)
    n<-length(timeseries1)
    rmse<-sqrt(sumsqerr/n)
    sdev <- sd(timeseries1,na.rm=TRUE)
    rsr <- rmse/sdev
  } else {rsr<-NA}
  return(rsr)
}

#' Function to return the standard deviation for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the standard deviation
#' 
#' @param x data frame containing value data for the chosen timeseries
#' @return sdev standard deviation for the given data frame
#' @examples
#' qfiletempf<-sampleData
#' sdev(qfiletempf$discharge)
sdev <- function(x) {
  sdev <- sd(x,na.rm=TRUE)
  return(sdev)
}