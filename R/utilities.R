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
#' @export
#' @examples
#' library(EflowStats)
#' flow_data<-obs_data
#' flow_data$date <- as.Date(flow_data$date)
#' flow_data <- dataCheck(flow_data, yearType = "water")
#' calculate_stat_flow_perc(flow_data, probs=c(.1,.25,.5,.75))
#' 
calculate_stat_flow_perc <- function(flow_data, probs=c(.1,.25,.5,.75,.9,.15)) {
  obs_percentiles <- quantile(flow_data$discharge,probs,na.rm=TRUE)
  return(obs_percentiles)
}

#' Function to return the Nash-Sutcliffe value between two data series
#' 
#' This function accepts two data frames containing daily data series 
#' and returns the Nash-Sutcliffe value
#' 
#' @param estimate_timeseries data frame continaing value data for the second chosen timeseries
#' @param reference_timeseries data frame containing value data for one of the chosen timeseries
#' @param na.rm Boolean defaults to TRUE.
#' @return nse Nash-Sutcliffe value between the two timeseries
#' @export
#' @examples
#' obs_data<-obs_data
#' mod_data<-mod_data
#' calculate_stat_nse(mod_data$discharge, obs_data$discharge)
#' 
calculate_stat_nse <-function(estimate_timeseries, reference_timeseries, na.rm=TRUE) {
  if (length(reference_timeseries) > 1) {
    numerat <- sum((reference_timeseries - estimate_timeseries)^2,na.rm=TRUE)
    denomin <- sum((reference_timeseries - mean(reference_timeseries, na.rm=TRUE))^2, na.rm = na.rm)
    nse <- (1 - (numerat / denomin))
  } else {nse <- NA} 
  return(nse)
}

#' Function to return the Nash-Sutcliffe value between the natural logarithms of the two data series
#' 
#' This function accepts two data frames containing daily data series and returns the Nash-Sutcliffe value of the natural 
#' logarithms of the data, with zeros removed.
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @param na.rm Boolean defaults to TRUE.
#' @return nselog Nash-Sutcliffe value between the natural log of the two timeseries
#' @export
#' @examples
#' obs_data<-obs_data
#' mod_data<-mod_data
#' calculate_stat_nselog(mod_data$discharge, obs_data$discharge)
#' 
calculate_stat_nselog<-function(estimate_timeseries, reference_timeseries, na.rm = TRUE) {
  # Count of zeros in dataset
  sszeros<-subset(reference_timeseries,reference_timeseries==0)
  
  if (length(sszeros)>0) {
    message(paste("\n", length(sszeros), 
                  "streamflows with a zero value were detected in this dataset.", 
                  "\nThese values will be removed before computing the \n",
                  "Nash-Sutcliffe efficiency value from the natural logs \n",
                  "of the streamflows."))
  }
  
  obsestq<-data.frame(reference_timeseries,estimate_timeseries, na.rm = TRUE)
  colnames(obsestq)<-c("obs","est")
  
  nozeros<-subset(obsestq,obsestq$obs>0)
  
  if (nrow(nozeros)>1) {
    
    nselog <- calculate_stat_nse(log(nozeros$est), 
                                 log(nozeros$obs), 
                                 na.rm = na.rm)

  } else {nselog<-NA}
  
  return(nselog)
}

#' Function to return the percent bias between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the 
#' percent bias.
#' 
#' @param estimate_timeseries data frame continaing value data for the second chosen timeseries
#' @param reference_timeseries data frame containing value data for one of the chosen timeseries
#' @return pbias percent bias between the two timeseries
#' @export
#' @examples
#' reference_timeseries<-obs_data$discharge
#' estimate_timeseries<-mod_data$discharge
#' calculate_stat_pbias(reference_timeseries,estimate_timeseries)
calculate_stat_pbias <- function (estimate_timeseries, reference_timeseries){
  
  denominator <- sum(reference_timeseries)
  
  if (denominator != 0) {
    
    pbias <- 100 * ( sum( estimate_timeseries - reference_timeseries ) / denominator )
    
  } else {
    pbias <- NA
    warning("'sum((obs)=0', it is not possible to compute 'pbias'")  
  }
  
  return(round(pbias, 1))
}

#' Function to return the root mean square error between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @param na.rm Boolean defaults to TRUE.
#' @return rmse root mean square error value between the two timeseries
#' @export
#' @examples
#' timeseries1<-obs_data$discharge
#' timeseries2<-mod_data$discharge
#' calculate_stat_rmse(timeseries1,timeseries2)
calculate_stat_rmse<-function(timeseries1, timeseries2, na.rm = TRUE) {
  if (length(timeseries1)>1) {
    rmse <- sqrt(mean((timeseries1 - timeseries2)^2, na.rm = na.rm))
  } else { rmse <- NA }
  return(rmse)
}

#' Function to return the normalized root mean square error between two data series
#' 
#' This function accepts two data frames containing daily data series and returns the normalized root mean square error
#' 
#' @param timeseries1 data frame containing value data for one of the chosen timeseries
#' @param timeseries2 data frame continaing value data for the second chosen timeseries
#' @return rmsne normalized root mean square error value between the two timeseries
#' @export
#' @examples
#' obs_data<-obs_data
#' mod_data<-mod_data
#' calculate_stat_rmsne(obs_data$discharge,mod_data$discharge)
#' 
calculate_stat_rmsne<-function(timeseries1,timeseries2) {
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
#' This function accepts observed and modeled daily data series and returns the 
#' root mean square error/standard deviation of the reference timeseries
#' 
#' @param estimate_timeseries data frame containing value data for the modeled timeseries
#' @param reference_timeseries data frame containing value data for the observed timeseries
#' @return rsr root mean square error/standard deviation for the two timeseries
#' @export
#' @examples
#' estimate_timeseries<-mod_data$discharge
#' reference_timeseries<-obs_data$discharge
#' calculate_stat_rsr(estimate_timeseries, reference_timeseries)
calculate_stat_rsr<-function(estimate_timeseries, reference_timeseries) {
  if (length(reference_timeseries)>1) {
    rmse <- calculate_stat_rmse(estimate_timeseries, reference_timeseries)
    sdev <- sd(reference_timeseries, na.rm=TRUE)
    if(sdev > 0) {
      rsr <- rmse/sdev
    } else {
      rsr <- NA
      warning("standard deviation of reference timeseries is 0, not possible to calculate rsr.")
    }
  } else {rsr<-NA}
  return(rsr)
}

#' Function to find peak flow and date of peak flow in a time series.
#' 
#' Accepts a \code{data.frame} as returned by \link[EflowStats]{dataCheck}
#' 
#' @param x data frame with date, discharge, year_val, and day columns. 
#' Can be constructed with \link[EflowStats]{dataCheck}
#' @param choose character with options "first" and "last". If multiple 
#' equal peaks are found the function will choose the first or last if
#' this is set. Otherwise, an error is returned for multiple peaks.
#' @return a \code{data.frame} with `peak` and `date` columns.
#' @export
#' @examples
#' library(EflowStats)
#' obs_data <- obs_data
#' obs_data$date <- as.Date(obs_data$date)
#' obs_data<-dataCheck(obs_data, yearType = "water")
#' find_peak_flow(obs_data)
#' 
find_peak_flow <- function(x, choose = "no") {
  year_vals <-unique(x$year_val)
  
  peak_flows <- data.frame(date = rep(as.Date("1970-01-01"), length(year_vals)),
                           peak = rep(NA, length(year_vals)))
  
  for(i in 1:length(year_vals)) {
    year_data <- x[c("date", "discharge")][which(x$year_val == year_vals[i]),]
    peak_inds <- which(year_data$discharge == max(year_data$discharge))
    if(length(peak_inds) > 1) {
      if(choose == "first") {
        warning(paste("Flow peaks found on more than one dates:\n", 
                      paste(x$date[peak_inds], collapse = ", "), 
                      "\n The first peak will be used."))
        peak_flows$peak[i] <- year_data$discharge[peak_inds[1]]
        peak_flows$date[i] <- year_data$date[peak_inds[1]]
      } else if(choose == "last") {
        warning(paste("Flow peaks found on more than one dates:\n", 
                      paste(x$date[peak_inds], collapse = ", "), 
                      "\n The last peak will be used."))
        peak_flows$peak[i] <- year_data$discharge[peak_inds[length(peak_inds)]]
        peak_flows$date[i] <- year_data$date[peak_inds[length(peak_inds)]]
      } else {
        stop(paste("Flow peaks found on more than one dates:\n", 
                   paste(x$date[peak_inds], collapse = ", "), 
                   "\n use choose input to select first or last."))
      }
    } else {
      peak_flows$peak[i] <- year_data$discharge[peak_inds]
      peak_flows$date[i] <- year_data$date[peak_inds]
    }
  }
  return(peak_flows)
}

