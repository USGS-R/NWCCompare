#' Function to return the OtherStats statistics for a given data series
#' 
#' This is a function to compute the 7 statistics of daily streamflow. Input to the function is a
#' time series of streamflow with date in the format Y-m-d. Output is a vector of OtherStats 
#' 1) mean, 2) median, 3) cv, 4) cv daily, 5) l7Q10, 6) l7Q2, 7) return_10 and 8) flow percentiles.
#' 
#' @param data data frame of daily flow data
#' @return OtherStats data frame of calculated statistics
#' @importFrom stats aggregate median sd
#' @export
#' @examples
#' timeseries1<-sampleData
#' calculate_other_flow_stats(timeseries1)
calculate_other_flow_stats<-function(data)  {

  sdbyyr <- aggregate(data$discharge, list(data$wy_val), 
                      sd)
  colnames(sdbyyr) <- c("Year", "sdq")
  meanbyyr <- aggregate(data$discharge, list(data$wy_val), 
                        mean, na.rm=TRUE)
  colnames(meanbyyr) <- c("Year", "meanq")
  medbyyr <- aggregate(data$discharge, list(data$wy_val), 
                       median, na.rm=TRUE)
  colnames(medbyyr) <- c("Year","medq")
  dfcvbyyr <- data.frame(meanbyyr$Year, sdbyyr$sdq, 
                         meanbyyr$meanq, medbyyr$medq,stringsAsFactors=FALSE)
  colnames(dfcvbyyr) <- c("Year", "sdq", "meanq", "medq")
  cvbyyr <- dfcvbyyr$sdq/dfcvbyyr$meanq
  dfcvbyyrf <- data.frame(dfcvbyyr, cvbyyr, stringsAsFactors=FALSE)
  colnames(dfcvbyyrf) <- c("Year", "sdq", "meanq", "medq", 
                           "cvq")
  
  mean_flow<-round(mean(dfcvbyyrf$meanq,na.rm=TRUE),digits=2)
  med_flow<-round(median(dfcvbyyrf$meanq,na.rm=TRUE),digits=2)
  cv_flow<-round(cv(dfcvbyyrf$meanq),digits=2)
  l7Q10v<-NWCportalL7Q10(data)
  l7Q2v<-NWCportalL7Q2(data)
  return_10v<-NWCportalReturn10(data)
  
  obs_percentiles <- flow_perc(data)
  flow_10 <- obs_percentiles[1]
  flow_25 <- obs_percentiles[2]
  flow_50 <- obs_percentiles[3]
  flow_75 <- obs_percentiles[4]
  flow_90 <- obs_percentiles[5]
  flow_15 <- obs_percentiles[6]
  OtherStatsv <- c(med_flow,cv_flow,l7Q10v,l7Q2v,return_10v,flow_10,flow_25,flow_50,flow_75,flow_90,flow_15)
  return(OtherStatsv)
}

#' Function to return the l7Q10 value for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the l7Q10 value, 
#' calculated for the National Water Census by a non-standard method.
#' 
#' @param qfiletempf data frame containing value data for one of the chosen timeseries
#' @return NWCportalL7Q10 l7Q10 value for the given data frame
#' @importFrom stats aggregate
#' @examples
#' qfiletempf<-sampleData
#' NWCportalL7Q10(qfiletempf)
NWCportalL7Q10 <- function(qfiletempf) {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                              "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                             list(rollingavgs7day$wy_val), min, na.rm=TRUE)
  sort_7day<-sort(min7daybyyear$x)
  rank_90<-floor(findrank(length(sort_7day),0.90))
  if (rank_90 > 0) { 
    l7Q10<-round(sort_7day[rank_90],digits=2)
  } else { 
    l7Q10<-NaN 
  }
  return(l7Q10)
}

#' Function to return the l7Q2 value for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the l7Q2 value, 
#' calculated for the National Water Census by a non-standard method.
#' 
#' @param qfiletempf data frame containing value data for one of the chosen timeseries
#' @return NWCportalL7Q2 l7Q2 value for the given data frame
#' @importFrom stats aggregate
#' @examples
#' qfiletempf<-sampleData
#' NWCportalL7Q2(qfiletempf)
NWCportalL7Q2 <- function(qfiletempf) {
  day7mean <- rollmean(qfiletempf$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(qfiletempf, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                              "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                             list(rollingavgs7day$wy_val), min, na.rm=TRUE)
  sort_7day<-sort(min7daybyyear$x)
  rank_50<-floor(findrank(length(sort_7day),0.50))
  if (rank_50 > 0) { 
    l7Q2<-round(sort_7day[rank_50],digits=2) 
  } else { 
    l7Q2<-NaN 
  }
  return(l7Q2)
}

#' Function to return the 10 year return value for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the 10 year return value, 
#' calculated for the National Water Census by a non-standard method.
#' 
#' @param qfiletempf data frame containing value data for the chosen timeseries
#' @return NWCportalReturn10 10-year return value for the given data frame
#' @importFrom stats aggregate
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
