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
calculate_other_flow_stats<-function(flow_data, digits = 3)  {

  mean_by_year <- aggregate(flow_data$discharge, list(flow_data$year_val), 
                        mean, na.rm=TRUE)[,2]
  
  mean_flow<-round(mean(mean_by_year, na.rm=TRUE), digits=digits)
  
  med_flow<-round(median(mean_by_year, na.rm=TRUE), digits=digits)
  
  cv_flow<-round(cv(mean_by_year), digits=digits)
  
  l7Q10v<-NWCportalL7Q10(flow_data)
  
  l7Q2v<-NWCportalL7Q2(flow_data)
  
  return_10v<-NWCportalReturn10(flow_data)
  
  obs_percentiles <- flow_perc(flow_data)
  
  flow_10 <- obs_percentiles[1]
  flow_25 <- obs_percentiles[2]
  flow_50 <- obs_percentiles[3]
  flow_75 <- obs_percentiles[4]
  flow_90 <- obs_percentiles[5]
  flow_15 <- obs_percentiles[6]
  
  OtherStatsv <- c(med_flow,cv_flow,l7Q10v,l7Q2v,return_10v,
                   flow_10,flow_25,flow_50,flow_75,flow_90,flow_15)

  return <- data.frame(indice = c("lam1","tau2","tau3","tau4","ar1","amplitude","phase"),
                        statistic = c(lam1,tau2,tau3,tau4,ar1v,amplitude,phase),
                        stringsAsFactors = F)
}

#' Function to return the l7Q10 value for a given data series
#' 
#' This function accepts a data frame containing daily data and returns the l7Q10 value, 
#' calculated for the National Water Census by a non-standard method.
#' 
#' @param flow_data data frame containing value data for one of the chosen timeseries
#' @return NWCportalL7Q10 l7Q10 value for the given data frame
#' @importFrom stats aggregate
#' @importFrom zoo rollmean
#' @examples
#' flow_data<-sampleData
#' NWCportalL7Q10(flow_data)
NWCportalL7Q10 <- function(flow_data, digits = 3) {
  day7mean <- rollmean(flow_data$discharge, 7, align = "right", fill = NA)
  day7rollingavg <- data.frame(flow_data, day7mean)
  rollingavgs7day <- subset(day7rollingavg, !is.na(day7rollingavg$day7mean))
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                             list(rollingavgs7day$year_val), min, na.rm=TRUE)
  sort_7day<-sort(min7daybyyear$x)
  rank_90<-floor(findrank(length(sort_7day),0.90))
  if (rank_90 > 0) { 
    l7Q10<-round(sort_7day[rank_90],digits=digits)
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
#' @param flow_data data frame containing value data for one of the chosen timeseries
#' @return NWCportalL7Q2 l7Q2 value for the given data frame
#' @importFrom stats aggregate
#' @importFrom zoo rollmean
#' @examples
#' flow_data<-sampleData
#' NWCportalL7Q2(flow_data)
NWCportalL7Q2 <- function(flow_data) {
  day7mean <- rollmean(flow_data$discharge, 7, align = "right", 
                       na.pad = TRUE)
  day7rollingavg <- data.frame(flow_data, day7mean)
  rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                              "NA")
  min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                             list(rollingavgs7day$year_val), min, na.rm=TRUE)
  sort_7day<-sort(min7daybyyear$x)
  rank_50<-floor(findrank(length(sort_7day),0.50))
  if (rank_50 > 0) { 
    l7Q2<-round(sort_7day[rank_50],digits=digits) 
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
#' @param flow_data data frame containing value data for the chosen timeseries
#' @return NWCportalReturn10 10-year return value for the given data frame
#' @importFrom stats aggregate
#' @examples
#' flow_data<-sampleData
#' NWCportalReturn10(flow_data)
NWCportalReturn10 <- function(flow_data) {
  annual_max <- aggregate(flow_data$discharge, list(flow_data$year_val), max, na.rm=TRUE)
  sort_annual_max <- sort(annual_max$x)
  rank_10 <- floor(findrank(length(sort_annual_max),0.10))
  return_10 <- sort_annual_max[rank_10]
  return(return_10)
}
