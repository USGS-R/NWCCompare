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

#' Function to transform data into the format required for calculation of HIT/HAT statistics
#' 
#' This function accepts a raw data frame pulled from the NWIS webservice and returns a data frame of 
#' observed data for use in calculating HIT/HAT statistics
#' 
#' @param x_obs data frame pulled from dataRetrieval::readNWISdv or NWCCompare::get_nwc_wb_data
#' @return obs_data data frame of observed data for the chosen parameters
#' @export
#' @examples
#' site<-"02177000"
#' startdate<-"2011-10-01"
#' enddate<-"2012-09-30"
#' \dontrun{
#' x_obs <- dataRetrieval::readNWISdv(site)
#' obs_data <- get_obsdata(x_obs)
#' }
get_obsdata <- function(x_obs) {
  if("agency_cd" %in% names(x_obs)) {
    x_obs <- data.frame(x_obs$Date, 
                        x_obs$X_00060_00003,
                        stringsAsFactors = FALSE)
  } else {
    x_obs<-data.frame(x_obs$date,x_obs$discharge,stringsAsFactors=FALSE)
  }
  colnames(x_obs)<-c("date","discharge")
  x_obs$month_val <- as.integer(substr(x_obs$date,6,7))
  x_obs$year_val <- as.integer(substr(x_obs$date,1,4))
  x_obs$day_val <- as.integer(substr(x_obs$date,9,10))
  x_obs$jul_val <- as.integer(strptime(x_obs$date,"%Y-%m-%d")$yday)+1
  x_obs$wy_val <- ifelse(as.numeric(x_obs$month_val)>=10,as.character(as.numeric(x_obs$year_val)+1),x_obs$year_val) 
  temp <- aggregate(discharge ~ wy_val,data=x_obs,length)
  temp <- temp[which(temp$discharge>=365),]
  obs_data<-x_obs[x_obs$wy_val %in% temp$wy_val,]
  return(obs_data)
}

# FlowStatsICP will be replaced by the EflowStats::hitStats, EflowStats::magnifSeven, and NWCCompare::FlowStatsICP

#' Function to run ICP HIT/HAT statistics for a given data set
#' 
#' This function accepts a data frame of daily flow data and returns a data frame of 
#' calculated HIT/HAT statistics 
#' 
#' @param data data frame of daily flow data
#' @param drain_area value of site drainage area
#' @return Output data frame of calculated statistics
#' @export
#' @importFrom EflowStats magnifSeven ma24.35 ma41.45 ml18 ml20 mh1.12 fl1.2 fh6 fh7 dl6 dl13 dl16.17 ta1.2 tl1.2 th1.2 ra5 ra7 ra8.9
#' @examples
#' drain_area<-54
#' qfiletempf<-sampleData
#' qfiletempf$date<-as.Date(qfiletempf$date,"%m/%d/%y")
#' FlowStatsICP(qfiletempf,drain_area)
FlowStatsICP <- function(data,drain_area) {
  dfOut <- vector()
  otherstat <- OtherStatsICP(data)
  dfOut <- c(dfOut,otherstat)
  magnif7 <- magnifSeven(data)
  dfOut <- c(dfOut,magnif7)
  
  ma26v<-ma24.35(data)[3,1]
  ma41v<-unlist(ma41.45(data,drain_area)[1])
  ml18v<-ml18(data)
  ml20v<-ml20(data)
  mh10v<-unlist(mh1.12(data)[10])
  dfOut <- c(dfOut,ma26v,ma41v,ml18v,ml20v,mh10v)
  
  fl2v<-unname(unlist(fl1.2(data)[2]))
  fh6v<-fh6(data)
  fh7v<-fh7(data)
  dfOut <- c(dfOut,fl2v,fh6v,fh7v)
  
  dl6v<-dl6(data)
  dl13v<-dl13(data)
  dl16v<-unname(unlist(dl16.17(data)[1]))
  dfOut <- c(dfOut,dl6v,dl13v,dl16v)
  
  ta1v<-unname(unlist(ta1.2(data)[1]))
  tl1v<-unname(unlist(tl1.2(data)[1]))
  th1v<-unname(unlist(th1.2(data)[1]))
  dfOut <- c(dfOut,ta1v,tl1v,th1v)
  
  ra5v<-ra5(data)
  ra7v<-ra7(data)
  ra8v<-unname(unlist(ra8.9(data)[1]))
  dfOut <- c(dfOut,ra5v,ra7v,ra8v)
  
  Output<-dfOut
  return(Output)
  
}