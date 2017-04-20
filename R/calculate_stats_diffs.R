#' Function to calculate the differences in statistics for two given datasets.
#' 
#' This function accepts two data frames of daily flow data and returns a data frame of 
#' calculated difference statistics from \link[EflowStats]{hitStats}, 
#' \link[EflowStats]{magnifSeven}, \link{calculate_other_flow_stats}, and \link{calculate_GoF_stats}.
#' HitStats, magnifSeven, and other_flow_stats are differented as \code{(a-b)/a} 
#' 
#' Assumptions:
#' Dates must match between the two data sources.
#' 
#' @param sites A two column dataframe containing site names for flow_data_a
#' and flow_data_b flow data.
#' @param flow_data_a A dataframe containing a NWCCompare flow dataset. 
#' Should have been cleaned by \link[EflowStats]{dataCheck} and constructed by
#' a build dataset function from this package.
#' @param flow_data_b A second NWCCompare flow dataset to be compared
#' to flow_data_a. 
#' @param yearType A charcter of either "water" or "calendar" indicating 
#' whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @return statsout data frame of calculated statistics
#' @importFrom stats aggregate
#' @export
#' @examples
#' # https://cida.usgs.gov/nwc/#!waterbudget/achuc/031300011004
#' nwis <- "02335757"
#' huc <- "031300011004"
#' sites <- data.frame(a=nwis, b=huc, stringsAsFactors = FALSE)
#' start_date <- "2004-10-01"
#' end_date <- "2010-09-30"
#' flow_data_a <- build_nwis_dv_dataset(nwis, start_date, end_date)
#' flow_data_b <- build_nwc_flow_dataset(huc, start_date, end_date)
#' diff_statsout <- calculate_stats_diffs(sites = sites, 
#'   flow_data_a = flow_data_a, 
#'   flow_data_b = flow_data_b, 
#'   yearType = "water",
#'   digits = 2) 
#' 
calculate_stats_diffs<-function(sites, flow_data_a, flow_data_b, 
                                yearType = "water", digits = 3) {
  
  if(!all(flow_data_a$daily_streamflow_cfs[[1]]$date == 
          flow_data_b$daily_streamflow_cfs[[1]]$date)) {
    stop("Dates in flow data do not match, can not proceed.")}
  
  stats=c("magAverage", "magLow", "magHigh",
          "frequencyLow", "frequencyHigh",
          "durationLow", "durationHigh",
          "timingAverage", "timingLow", "timingHigh",
          "rateChange",
          "magnifSeven", "otherStat")
  
  min_date <- as.character(min(flow_data_a$daily_streamflow_cfs[[1]]$date))
  max_date <- as.character(max(flow_data_a$daily_streamflow_cfs[[1]]$date))
  
  hitStats_result_a <- calculate_stats_by_group(stats = stats, 
                                                flow_data = flow_data_a, 
                                                yearType = yearType, 
                                                digits = digits)
  hitStats_result_b <- calculate_stats_by_group(stats = stats, 
                                                flow_data = flow_data_b, 
                                                yearType = yearType, 
                                                digits = digits)
  
  GoFstats_results <- rep(list(list()), nrow(sites))
  for (i in 1:nrow(sites)) {

    site_a <- sites[i,1]
    site_b <- sites[i,2]


    GoFstats_results_site <- calculate_GoF_stats(flow_data_b$daily_streamflow_cfs[site_b][[1]],
                                                 flow_data_a$daily_streamflow_cfs[site_a][[1]])
    if(i==1) {
      statsout <- cbind(hitStats_result_a,GoFstats_results_site)
      init <- FALSE
    }
    
    statsout[i,4:(ncol(hitStats_result_a)-1)] <- (hitStats_result_a[i,4:(ncol(hitStats_result_a)-1)] - 
                                               hitStats_result_b[i,4:(ncol(hitStats_result_a)-1)]) / 
                                              hitStats_result_a[i,4:(ncol(hitStats_result_a)-1)]
    statsout[i,(ncol(hitStats_result_a)+1):ncol(statsout)] <- GoFstats_results_site
  }
  return(statsout)
}
