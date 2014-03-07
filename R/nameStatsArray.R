#' Function to generate names for the stats in tempArrays
#' 
#' @param stats string containing stat groups desired
#' @param sites list of usgs station ids
#' @param tempArrays list of stats arrays
#' @return statsout data frame of calculated statistics
#' @export
#' @examples
#' stats<-"magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
#' sites<-"031401020800,031401020800"
#' sites<-read.csv(header=F,colClasses=c("character"),text=sites)
#' tempArrays<-getEmptyResultArrayNWCStats(stats, 1, getSupportedStatNames())
#' nameStatsArray(stats, sites, tempArrays)
nameStatsArray<-function(stats, sites, tempArrays){
  supportedStats=getSupportedStatNames()
  if (stats=="GoF") {
    statsout <- data.frame(t(sites),tempArrays$min_date, tempArrays$max_date, tempArrays$ObsStats, tempArrays$ModStats, tempArrays$DiffStats, tempArrays$GoFStats, tempArrays$comment, stringsAsFactors = FALSE)
    namesFull <- c("site_no","min_date","max_date")
    namesFull <- c(namesFull,supportedStats$namesCompareStatObs,supportedStats$namesCompareStatMod,supportedStats$namesCompareStatDiff,supportedStats$namesGoFStat,"comment")
  } else {
  statsout <- data.frame(t(sites),tempArrays$min_date, tempArrays$max_date, tempArrays$magnifSevenObs, tempArrays$ObsFlowStats, tempArrays$comment, stringsAsFactors = FALSE)
  
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
  }
  colnames(statsout) <- namesFull
  return(statsout)
}