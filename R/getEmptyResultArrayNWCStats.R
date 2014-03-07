#' Function to generate empty list of Arrays to use for stats
#' 
#' This function accepts observed and modeled data frames of daily flow data and returns a data frame of 
#' calculated diff statistics
#' 
#' @param stats string containing stat groups desired
#' @param numSites number of sites in sites string
#' @param supportedStats list of all supported names
#' @return emptyArrays list of empty arrays for use with stats
#' @export
#' @examples
#' emptyArrays<-getEmptyResultArrayNWCStats("GoF", 2, getSupportedStatNames())
getEmptyResultArrayNWCStats <- function(stats, numSites, supportedStats){
  if (stats=="GoF") {
    Flownum <- length(supportedStats$namesCompareStatObs)
    ObsStats <- matrix(nrow=numSites,ncol=Flownum)
    ModStats <- matrix(nrow=numSites,ncol=Flownum)
    GofNum <- length(supportedStats$namesGoFStat)
    GoFStats <- matrix(nrow=numSites,ncol=GofNum)
    DiffNum <- length(supportedStats$namesCompareStatDiff)
    DiffStats <- matrix(nrow=numSites,ncol=DiffNum)
    min_date <- vector(length = numSites)
    max_date <- vector(length = numSites)
    comment <- vector(length = numSites)
    emptyArrays <- list("ObsStats"=ObsStats,"ModStats"=ModStats,"GoFStats"=GoFStats,"DiffStats"=DiffStats,"min_date"=min_date,
                        "max_date"=max_date,
                        "comment"=comment)
  } else {
  Flownum <- (length(grep("magStat", stats)) * length(supportedStats$namesMagStat)) +
    (length(grep("flowStat", stats)) * length(supportedStats$namesFlowStat)) +
    (length(grep("durStat", stats)) * length(supportedStats$namesDurStat)) +
    (length(grep("timStat", stats)) * length(supportedStats$namesTimStat)) +
    (length(grep("rateStat", stats)) * length(supportedStats$namesRateStat)) +
    (length(grep("otherStat", stats)) * length(supportedStats$namesOtherStat))
  Magnifnum <- (length(grep("magnifSeven", stats)) * length(supportedStats$namesMagnif))
  ObsFlowStats <- matrix(nrow = numSites, ncol = Flownum)
  magnifSevenObs <- matrix(nrow = nrow(ObsFlowStats), ncol = Magnifnum)
  min_date <- vector(length = numSites)
  max_date <- vector(length = numSites)
  comment <- vector(length = numSites)
  emptyArrays <- list("ObsFlowStats"=ObsFlowStats,
                      "magnifSevenObs"=magnifSevenObs,
                      "min_date"=min_date,
                      "max_date"=max_date,
                      "comment"=comment)
  }
  return(emptyArrays)
}