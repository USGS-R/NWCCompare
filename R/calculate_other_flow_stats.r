#' Function to return the OtherStats statistics for a given data series
#' 
#' This is a function to compute the 7 statistics of daily streamflow. Input to the function is a
#' time series of streamflow with date in the format Y-m-d. Output is a vector of OtherStats 
#' 1) mean, 2) median, 3) cv, 4) cv daily, 5) l7Q10, 6) l7Q2, 7) return_10 and 8) flow percentiles.
#' 
#' @param flow_data A dataframe containing a NWCCompare flow dataset. 
#' Should have been cleaned by \link[EflowStats]{dataCheck}
#' @param digits A numeric. Number of digits to round indice values
#' @return OtherStats data frame of calculated statistics
#' @importFrom stats aggregate median sd
#' @export
#' @examples
#' timeseries1<-sampleData
#' calculate_other_flow_stats(timeseries1)
calculate_other_flow_stats<-function(flow_data, digits = 3)  {

  mean_by_year <- aggregate(flow_data$discharge, list(flow_data$year_val), 
                        mean, na.rm=TRUE)[,2]
  
  med_flow<-round(median(mean_by_year, na.rm=TRUE), digits=digits)
  
  cv_flow<-round(cv(mean_by_year), digits=digits)
  
  cv_daily<-round(cv(flow_data$discharge),digits=digits)
  
  obs_percentiles <- flow_perc(flow_data)
  flow_10 <- obs_percentiles[1]
  flow_25 <- obs_percentiles[2]
  flow_50 <- obs_percentiles[3]
  flow_75 <- obs_percentiles[4]
  flow_90 <- obs_percentiles[5]
  flow_15 <- obs_percentiles[6]

  return <- data.frame(indice = c("med_flow", "cv_flow", "cv_daily", 
                                  "flow_10", "flow_25", "flow_50", 
                                  "flow_75", "flow_90", "flow_15"),
                        statistic = c(med_flow,cv_flow,cv_daily,
                                      flow_10,flow_25,flow_50,
                                      flow_75,flow_90,flow_15),
                        stringsAsFactors = F)
}
