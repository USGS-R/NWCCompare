#' Function to calculate statistics of daily flow by group for many time series
#' 
#' This function traverses a collection of daily streamflow data and 
#' calculates the requested statistics for each time series. Available 
#' statistics include those implemented by \link[EflowStats]{calc_allHIT}, 
#' \link[EflowStats]{calc_magnifSeven}, \link{calculate_other_flow_stats}.
#' 
#' @param stats string containing stat groups desired. 
#' options are: "all", "magAverage", "magLow", "magHigh", "frequencyLow", 
#' "frequencyHigh", "durationLow", "durationHigh", "timingAverage", 
#' "timingLow", "timingHigh", "rateChange", "calc_magnifSeven", "otherStat"
#' @param flow_data A dataframe containing a NWCCompare flow dataset. 
#' Should have been cleaned by \link[EflowStats]{validate_data}
#' @param yearType A charcter of either "water" or "calendar" indicating 
#' whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @return Data frame of calculated statistics. Time series ids are used 
#' as row names, statistic ids as column names.
#' @importFrom EflowStats calc_magnifSeven calc_allHIT validate_data
#' @importFrom stats aggregate
#' @export
#' @examples
#' sites <- c("02177000","02178400")
#' startdate <- "2008-10-01"
#' enddate <- "2013-09-30"
#' nwis_dataset <- build_nwis_dv_dataset(sites, startdate, enddate)
#' stats=c("calc_magAverage", "calc_magLow", "calc_magHigh",
#'         "calc_frequencyLow", "calc_frequencyHigh",
#'         "calc_durationLow", "calc_durationHigh",
#'         "calc_timingAverage", "calc_timingLow", "calc_timingHigh",
#'         "calc_rateChange",
#'         "calc_magnifSeven", "otherStat")
#' eflow_stats <- calculate_stats_by_group(stats, nwis_dataset)
calculate_stats_by_group<-function(stats, flow_data, 
                                   yearType = "water", digits = 3) {
  
  if("calc_magnifSeven" %in% stats || "all" %in% stats) {
    stats <- stats[!stats %in% "calc_magnifSeven"]
    mag7 <- TRUE
  }
  
  if("otherStat" %in% stats || "all" %in% stats) {
    stats <- stats[!stats %in% "otherStat"]
    ostat <- TRUE
  }
  
  sites <- names(flow_data$daily_streamflow_cfs)
  
  init <- TRUE
  
  min_date <- rep("", length(sites))
  names(min_date) <- sites
  max_date <- min_date
  
  for (site in sites) {
    
    drainage_area <- flow_data$drainage_area_sqmi[site][[1]]
    
    flood_threshold <- flow_data$peak_threshold_cfs[site][[1]]
    
    flow_data_site <- flow_data$daily_streamflow_cfs[site][[1]]
    
    calc_allHIT_result <- calc_allHIT(flow_data_site,
                                drainArea=drainage_area,
                                floodThreshold=flood_threshold,
                                stats = stats,
                                yearType = yearType)
    
    min_date[site] <- as.character(min(flow_data_site$date))
    max_date[site] <- as.character(max(flow_data_site$date))
    
    nrows_out <- nrow(calc_allHIT_result)
    
    if (mag7) {
      mag7_result <- calc_magnifSeven(flow_data_site, yearType, digits)
      nrows_out <- nrows_out + nrow(mag7_result)
    }
    
    if (ostat) {
      ostat_result <- calculate_other_flow_stats(flow_data = flow_data_site, digits = digits)
      nrows_out <- nrows_out + nrow(ostat_result)
    }
    
    if(init) {
      output <- data.frame(matrix(ncol = (length(sites) + 1), nrow = nrows_out))
      names(output) <- c("indice", sites)
      
      output[,1:2] <- rbind(mag7_result, ostat_result, calc_allHIT_result)
      
      output[1:7,1] <- c("lam1","tau2","tau3",
                         "tau4","ar1","amplitude","phase")
      output[8:16,1] <- c("med_flow", "cv_flow", "cv_daily", 
                           "flow_10", "flow_25", "flow_50", 
                           "flow_75", "flow_90", "flow_15")
      
      init = FALSE
    } else {
      
      output[site] <- rbind(mag7_result, ostat_result, calc_allHIT_result)["statistic"]
      
    }
  }
  
  statsout <- data.frame(matrix(NA, nrow=length(sites), ncol = (3 + nrow(output) + 1)))
  names(statsout) <- c("site_no", "min_date", "max_date", output[,1], "comment")
  
  statsout[,1] <- sites
  statsout[,2] <- min_date
  statsout[,3] <- max_date
  for(i in 1:length(sites)) {
    statsout[i,4:(length(statsout)-1)] <- output[,(i+1)]
  }
  statsout[1,length(statsout)] <- ""
  
  return(statsout)
}