calculateStatsGroupsSWE<-function(stats, sites, sos, observedProperty, wfs_url, wfsTypename, wfsProperty,wfsPropertyname) {
  sos<-paste(sos,'?request=GetObservation&service=SOS&version=1.0.0&observedProperty=',observedProperty,'&offering=REPLACE_SITE',sep="")
  supportedStats=getSupportedStatNames()
  tempArrays<-getEmptyResultArrayNWCStats(stats, length(sites), supportedStats)
  for (i in 1:length(sites)) {
    site = sites[i]
    sos_url<-gsub('REPLACE_SITE',site,sos)
    x_data <- SWE_CSV_IHA(sos_url)
    drain_area<-getWFSFieldAsNumeric(wfs_url,wfsTypename,wfsProperty,site,wfsPropertyname)
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
  }
  
  statsout <- data.frame(sites, tempArrays$min_date, tempArrays$max_date, tempArrays$magnifSevenObs, tempArrays$ObsFlowStats, tempArrays$comment, stringsAsFactors = FALSE)
  
  namesFull <- c("site_no", "min_date", "max_date")
  
  if (grepl("magnifSeven", stats)) {
    namesFull <- c(namesFull, supportedStats$namesMagnif)
  }
  if (grepl("otherStat", stats)) {
    namesFull <- c(namesFull, supportedStats$namesOtherStat)
  }
  if (grepl("magStat", stats)) {
    namesFull <- c(namesFull, supportedStats$namesMagStat)
  }
  if (grepl("flowStat", stats)) {
    namesFull <- c(namesFull, supportedStats$namesFlowStat)
  }
  if (grepl("durStat", stats)) {
    namesFull <- c(namesFull, supportedStats$namesDurStat)
  }
  if (grepl("timStat", stats)) {
    namesFull <- c(namesFull, supportedStats$namesTimStat)
  }
  if (grepl("rateStat", stats)) {
    namesFull <- c(namesFull, supportedStats$namesRateStat)
  }
  namesFull <- c(namesFull, "comment")
  
  colnames(statsout) <- namesFull
  return(statsout)
}