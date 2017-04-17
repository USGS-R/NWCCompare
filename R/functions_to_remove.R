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
#' sites<-read.csv(header=FALSE,colClasses=c("character"),text=sites)
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

#' Function to generate list of al possible names
#' 
#' This function accepts observed and modeled data frames of daily flow data and returns a data frame of 
#' calculated diff statistics
#' 
#' @return supportedNames list of all names supported by the package
#' @export
getSupportedStatNames <- function() {
  namesMagnif <- c("lam1Obs", "tau2Obs", "tau3Obs", "tau4Obs", "ar1Obs", "amplitudeObs", "phaseObs")
  namesMagStat <- c(paste('ma',sequence(45),sep=""), paste('ml',sequence(22),sep=""),paste('mh',sequence(27),sep=""))
  namesFlowStat <- c(paste('fl',sequence(3),sep=""),paste('fh',sequence(11),sep=""))
  namesDurStat <- c(paste('dl',sequence(20),sep=""),paste('dh',sequence(24),sep=""))
  namesTimStat <- c(paste('ta',sequence(3),sep=""),paste('tl',sequence(4),sep=""),paste('th',sequence(3),sep=""))
  namesRateStat <- c(paste('ra',sequence(9),sep=""))
  namesOtherStatICP <- c("NWCportalL7Q10Obs", "NWCportalL7Q2Obs", "NWCportalReturn10Obs")
  namesOtherStat <- c("med_flowObs", "cv_flowObs", "cv_dailyObs", "flow_10Obs", "flow_25Obs", "flow_50Obs", "flow_75Obs", "flow_90Obs", "flow_15Obs")
  namesCompareStatObs <- c("lam1Obs", "tau2Obs", "tau3Obs", "tau4Obs", "ar1Obs", "amplitudeObs", "phaseObs","med_flowObs",
                           "cv_flowObs","l7Q10Obs","l7Q2Obs","return_10Obs","flow_10Obs","flow_25Obs","flow_50Obs",
                           "flow_75Obs","flow_90Obs","flow_15Obs","ma26Obs","ma41Obs","ml18Obs","ml20Obs","mh10Obs",
                           "fl2Obs","fh6Obs","fh7Obs","dl6Obs","dl13Obs","dl16Obs","ta1Obs","tl1Obs","th1Obs","ra5Obs",
                           "ra7Obs","ra8Obs")
  namesCompareStatMod <- c("lam1Mod", "tau2Mod", "tau3Mod", "tau4Mod", "ar1Mod", "amplitudeMod", "phaseMod","med_flowMod",
                           "cv_flowMod","l7Q10Mod","l7Q2Mod","return_10Mod","flow_10Mod","flow_25Mod","flow_50Mod",
                           "flow_75Mod","flow_90Mod","flow_15Mod","ma26Mod","ma41Mod","ml18Mod","ml20Mod","mh10Mod",
                           "fl2Mod","fh6Mod","fh7Mod","dl6Mod","dl13Mod","dl16Mod","ta1Mod","tl1Mod","th1Mod","ra5Mod",
                           "ra7Mod","ra8Mod")
  namesCompareStatDiff <- c("lam1Diff", "tau2Diff", "tau3Diff", "tau4Diff", "ar1Diff", "amplitudeDiff", "phaseDiff",
                            "med_flowDiff","cv_flowDiff","l7Q10Diff","l7Q2Diff","return_10Diff","flow_10Diff","flow_25Diff",
                            "flow_50Diff","flow_75Diff","flow_90Diff","flow_15Diff","ma26Diff","ma41Diff","ml18Diff","ml20Diff","mh10Diff",
                            "fl2Diff","fh6Diff","fh7Diff","dl6Diff","dl13Diff","dl16Diff","ta1Diff","tl1Diff","th1Diff","ra5Diff",
                            "ra7Diff","ra8Diff")
  namesGoFStat <- c("nse","nselog","rmse","rmsne","rsr","pbias","pearson","spearman",'nse_90','nse_75_90','nse_50_75','nse_25_50','nse_10_25',
                    'nse_10','rmse_90','rmse_75_90','rmse_50_75','rmse_25_50','rmse_10_25','rmse_10','rmsne_90','rmsne_75_90','rmsne_50_75',
                    'rmsne_25_50','rmsne_10_25','rmsne_10','rsr_90','rsr_75_90','rsr_50_75','rsr_25_50','rsr_10_25','rsr_10','pbias_90',
                    'pbias_75_90','pbias_50_75','pbias_25_50','pbias_10_25','pbias_10','pearson_90','pearson_75_90','pearson_50_75',
                    'pearson_25_50','pearson_10_25','pearson_10','spearman_90','spearman_75_90','spearman_50_75','spearman_25_50',
                    'spearman_10_25','spearman_10','NSEbyMonthJan','NSELOGbyMonthJan','RMSEbyMonthJan','RMSNEbyMonthJan','RSRbyMonthJan',
                    'BiasbyMonthJan','PearsonbyMonthJan','SpearmanbyMonthJan','NSEbyMonthFeb','NSELOGbyMonthFeb','RMSEbyMonthFeb',
                    'RMSNEbyMonthFeb','RSRbyMonthFeb','BiasbyMonthFeb','PearsonbyMonthFeb','SpearmanbyMonthFeb','NSEbyMonthMar',
                    'NSELOGbyMonthMar','RMSEbyMonthMar','RMSNEbyMonthMar','RSRbyMonthMar','BiasbyMonthMar','PearsonbyMonthMar','SpearmanbyMonthMar',
                    'NSEbyMonthApr','NSELOGbyMonthApr','RMSEbyMonthApr','RMSNEbyMonthApr','RSRbyMonthApr','BiasbyMonthApr','PearsonbyMonthApr',
                    'SpearmanbyMonthApr','NSEbyMonthMay','NSELOGbyMonthMay','RMSEbyMonthMay','RMSNEbyMonthMay','RSRbyMonthMay','BiasbyMonthMay',
                    'PearsonbyMonthMay','SpearmanbyMonthMay','NSEbyMonthJun','NSELOGbyMonthJun','RMSEbyMonthJun','RMSNEbyMonthJun',
                    'RSRbyMonthJun','BiasbyMonthJun','PearsonbyMonthJun','SpearmanbyMonthJun','NSEbyMonthJul','NSELOGbyMonthJul',
                    'RMSEbyMonthJul','RMSNEbyMonthJul','RSRbyMonthJul','BiasbyMonthJul','PearsonbyMonthJul','SpearmanbyMonthJul','NSEbyMonthAug',
                    'NSELOGbyMonthAug','RMSEbyMonthAug','RMSNEbyMonthAug','RSRbyMonthAug','BiasbyMonthAug','PearsonbyMonthAug','SpearmanbyMonthAug',
                    'NSEbyMonthSep','NSELOGbyMonthSep','RMSEbyMonthSep','RMSNEbyMonthSep','RSRbyMonthSep','BiasbyMonthSep','PearsonbyMonthSep','SpearmanbyMonthSep',
                    'NSEbyMonthOct','NSELOGbyMonthOct','RMSEbyMonthOct','RMSNEbyMonthOct','RSRbyMonthOct','BiasbyMonthOct','PearsonbyMonthOct','SpearmanbyMonthOct',
                    'NSEbyMonthNov','NSELOGbyMonthNov','RMSEbyMonthNov','RMSNEbyMonthNov','RSRbyMonthNov','BiasbyMonthNov','PearsonbyMonthNov','SpearmanbyMonthNov',
                    'NSEbyMonthDec','NSELOGbyMonthDec','RMSEbyMonthDec','RMSNEbyMonthDec','RSRbyMonthDec','BiasbyMonthDec','PearsonbyMonthDec','SpearmanbyMonthDec')
  supportedNames <- list("namesMagnif" = namesMagnif, 
                         "namesMagStat" = namesMagStat, 
                         "namesFlowStat" = namesFlowStat, 
                         "namesDurStat" = namesDurStat, 
                         "namesTimStat" = namesTimStat, 
                         "namesRateStat" = namesRateStat, 
                         "namesOtherStat" = namesOtherStat,
                         "namesOtherStatICP" = namesOtherStatICP, 
                         "namesGoFStat" = namesGoFStat,
                         "namesCompareStatObs"=namesCompareStatObs,
                         "namesCompareStatMod"=namesCompareStatMod,
                         "namesCompareStatDiff"=namesCompareStatDiff)
  return(supportedNames)
}

#' Function to return the drainage area as a numeric
#' 
#' @param wfs_url url of WFS
#' @param wfsTypename type to paste into url
#' @param wfsProperty property to paste into url
#' @param wfsLiteral site_no to paste into url
#' @param wfsPropertyname property name to paste into url
#' @return drain_area numeric of site drainage area
#' @export
#' @importFrom utils read.delim
#' @examples
#' wfs_url<-'http://cida.usgs.gov/nwc/geoserver/NWC/ows'
#' wfsTypename='NWC:huc12_SE_Basins_v2'
#' wfsProperty='NWC:HUC12'
#' wfsPropertyname='NWC:mi2'
#' wfsLiteral='031401020800'
#' area<-getWFSFieldAsNumeric(wfs_url,wfsTypename,wfsProperty,wfsLiteral,wfsPropertyname)
getWFSFieldAsNumeric<-function(wfs_url,wfsTypename,wfsProperty,wfsLiteral,wfsPropertyname){
  service_string <- '?service=WFS&version=1.0.0&request=GetFeature&typeName=TYPENAME_REPLACE&outputFormat=csv&filter=%3Cogc:Filter%20xmlns:ogc=%22http://www.opengis.net/ogc%22%3E%20%3Cogc:PropertyIsEqualTo%3E%20%3Cogc:PropertyName%3EPROPERTY_REPLACE%3C/ogc:PropertyName%3E%3Cogc:Literal%3ELITERAL_REPLACE%3C/ogc:Literal%3E%3C/ogc:PropertyIsEqualTo%3E%20%3C/ogc:Filter%3E&propertyName=PROPERTYNAME_REPLACE'
  wfs_url <- paste(wfs_url,service_string,sep="")
  wfs_url <- gsub('TYPENAME_REPLACE',wfsTypename,wfs_url)
  wfs_url <- gsub('PROPERTY_REPLACE',wfsProperty,wfs_url)
  wfs_url <- gsub('LITERAL_REPLACE',wfsLiteral,wfs_url)
  wfs_url <- gsub('PROPERTYNAME_REPLACE',wfsPropertyname,wfs_url)
  if ( grepl(':',wfsPropertyname)){
    wfs_col<-strsplit(wfsPropertyname,':')[[1]][2]
  } else {
    wfs_col<-wfsPropertyname
  }
  drain_area <- as.numeric(read.delim(wfs_url, header=TRUE, sep=",", colClasses=c("character"))[wfs_col]) * 0.386102 # Note that this is in square miles as required by underlying package.
  return(drain_area)
}