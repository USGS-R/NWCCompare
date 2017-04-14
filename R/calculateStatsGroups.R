#' Function to calculate the statistics for a given data set
#' 
#' This function accepts observed and modeled data frames of daily flow data and returns a data frame of 
#' calculated diff statistics
#' 
#' @param stats string containing stat groups desired
#' @param sites list of usgs station ids
#' @param startdate startdate for pulling data
#' @param enddate enddate for pulling data
#' @param X_DATA_FUN function for pulling data from x_args
#' @param x_args urls for pulling observed data
#' @param DRAIN_AREA_FUN function for pulling drainage area
#' @param drain_args url for pulling drainage area
#' @param drain_site_param string for use w/ multiple drain urls
#' @return statsout data frame of calculated statistics
#' @importFrom EflowStats magnifSeven FlowStatsAll
#' @export
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
      countbyyr<-aggregate(flow_data$discharge, list(flow_data$wy_val), length)
      colnames(countbyyr)<-c('wy','num_samples')
      sub_countbyyr<-subset(countbyyr,num_samples >= 365)
      if (nrow(sub_countbyyr)==0) {
        tempArrays$comment[i]<-"No complete water years for site"
      } else {
        flow_data<-merge(flow_data,sub_countbyyr,by.x="wy_val",by.y="wy")
        flow_data<-flow_data[order(flow_data$jul_val),]
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
    }} else {
      tempArrays$comment[i] <- "No observed data for this site"
    }
    #tempArrays<-runStatsGroups(x_data,tempArrays,i,drain_area)
  }
  statsout<-nameStatsArray(stats, sites, tempArrays)
  return(statsout)
}