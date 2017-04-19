#' Function to calculate statistics of daily flow by grtoup for many sites
#' 
#' This function traverses a collection of daily streamflow data and 
#' calculate the requested statistics for each site. Available statistics
#' include those implemented by \link[EflowStats]{hitStats}, 
#' \link[EflowStats]{magnifSeven}, \link{functioname}
#' 
#' @param stats string containing stat groups desired
#' @param flow_data A dataframe containing a NWCCompare flow dataset.
#' @param yearType A charcter of either "water" or "calendar" indicating 
#' whether to use water years or calendar years, respectively.
#' @param digits A numeric. Number of digits to round indice values
#' @return Data frame of calculated statistics. Time series ids are used 
#' as row names, statistic ids as column names.
#' @importFrom EflowStats magnifSeven hitStats dataCheck
#' @importFrom stats aggregate
#' @export
#' @examples
#' sites <- c("02177000","02178400")
#' startdate <- "2008-10-01"
#' enddate <- "2013-09-30"
#' nwis_dataset <- build_nwis_dv_dataset(sites, startdate, enddate)
#' stats=c("magAverage", "magLow", "magHigh", 
#'         "frequencyLow", "frequencyHigh", 
#'         "durationLow", "durationHigh", 
#'         "timingAverage", "timingLow", "timingHigh", 
#'         "rateChange", 
#'         "magnifSeven")
#' eflow_stats <- calculate_stats_by_group(stats, nwis_dataset)
calculate_stats_by_group<-function(stats, flow_data, yearType = "water", digits = 2) {
  
  if("magnifSeven" %in% stats) {
    stats <- stats[!stats %in% "magnifSeven"]
    mag7 <- TRUE
  }
  
  supportedStats=getSupportedStatNames()
  oldstats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat,otherStat"
  refArray <- getEmptyResultArrayNWCStats(oldstats, length(sites), supportedStats)
  
  sites <- names(flow_data$daily_streamflow_cfs)
  
  init <- TRUE
  
  for (site in sites) {
    
    drainage_area <- flow_data$drainage_area_sqmi[site][[1]]
    
    flood_threshold <- flow_data$peak_threshold_cfs[site][[1]]
    
    flow_data_site <- flow_data$daily_streamflow_cfs[site][[1]]
    
    hitStats_result <- hitStats(flow_data_site,
                                drainArea=drainage_area,
                                floodThreshold=flood_threshold,
                                stats = stats,
                                yearType = yearType)
    
    min_date <- as.character(min(flow_data_site$date))
    max_date <- as.character(max(flow_data_site$date))
    
    if (mag7) {
      mag7_result <- magnifSeven(flow_data_site, yearType, digits)
    }
    
    if(init) {
      output <- rbind(mag7_result, hitStats_result)
      names(output) <- c("indice", site)
      init = FALSE
    } else {
      output[site] <- rbind(mag7_result, hitStats_result)["statistic"]
    }
  }
  return(output)
}