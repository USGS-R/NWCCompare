nameStatsArray<-function(stats, sites, tempArrays){
  supportedStats=getSupportedStatNames()
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