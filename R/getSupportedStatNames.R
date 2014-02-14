getSupportedStatNames <- function() {
  namesMagnif <- c("lam1Obs", "tau2Obs", "tau3Obs", "tau4Obs", "ar1Obs", "amplitudeObs", "phaseObs")
  namesMagStat <- c(paste('ma',sequence(45),sep=""), paste('ml',sequence(22),sep=""),paste('mh',sequence(27),sep=""))
  namesFlowStat <- c(paste('fl',sequence(3),sep=""),paste('fh',sequence(10),sep=""))
  namesDurStat <- c(paste('dl',sequence(20),sep=""),paste('dh',sequence(21),sep=""))
  namesTimStat <- c(paste('ta',sequence(2),sep=""),paste('tl',sequence(2),sep=""),paste('th',sequence(2),sep=""))
  namesRateStat <- c(paste('ra',sequence(9),sep=""))
  namesOtherStat <- c("med_flowObs", "cv_flowObs", "cv_dailyObs", "l7Q10Obs", "l7Q2Obs", "return_10Obs", "flow_10Obs", "flow_25Obs", "flow_50Obs", "flow_75Obs", "flow_90Obs", "flow_15Obs")
  supportedNames <- list("namesMagnif" = namesMagnif, 
                         "namesMagStat" = namesMagStat, 
                         "namesFlowStat" = namesFlowStat, 
                         "namesDurStat" = namesDurStat, 
                         "namesTimStat" = namesTimStat, 
                         "namesRateStat" = namesRateStat, 
                         "namesOtherStat" = namesOtherStat)
  return(supportedNames)
}