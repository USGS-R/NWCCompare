calculateStatsGroups<-function(stats, sites, startdate, enddate, X_DATA_FUN, x_args, DRAIN_AREA_FUN, drain_args, drain_site_param=NULL) {
  supportedStats=getSupportedStatNames()
  tempArrays<-getEmptyResultArrayNWCStats(stats, length(sites), supportedStats)
  for (i in 1:length(sites)) {
    site = sites[i]
    x_data <- X_DATA_FUN(x_args[i])
    if (!is.null(drain_site_param)) { 
      # In the case that the drainage area function input is multivalued with one that varies. (The url is constructed in the function)
      drain_args[[drain_site_param]]<-as.character(site)
      drain_area <- do.call(DRAIN_AREA_FUN,drain_args)
    } else
    {
      # In the case th drainage area function input is single valued (a list of urls)
      drain_area<-DRAIN_AREA_FUN(drain_args[i])
    }
    if (nrow(x_data) > 2) {
      flow_data <- get_obsdata(x_data)
      tempArrays$min_date[i] <- as.character(min(flow_data$date))
      tempArrays$max_date[i] <- as.character(max(flow_data$date))
      flow_data <- flow_data[, c("wy_val", "date", "discharge", "month_val", "year_val", "day_val", "jul_val")]
      if (ncol(tempArrays$ObsFlowStats) > 0) {
        tempArrays$ObsFlowStats[i, ] <- FlowStatsAll(flow_data, drain_area, stats=stats)
      }
      if (ncol(tempArrays$magnifSevenObs) > 0) {
        tempArrays$magnifSevenObs[i, ] <- magnifSeven(flow_data)
      }
      tempArrays$comment <- ""
    } else {
      tempArrays$comment[i] <- "No observed data for this site"
    }
    #tempArrays<-runStatsGroups(x_data,tempArrays,i,drain_area)
  }
  statsout<-nameStatsArray(stats, sites, tempArrays)
  return(statsout)
}