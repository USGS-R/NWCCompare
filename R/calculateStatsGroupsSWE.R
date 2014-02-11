calculateStatsGroupsSWE<-function(stats, sites, sos, observedProperty, wfs_url, wfsTypename, wfsProperty,wfsPropertyname) {
  sos<-paste(sos,'?request=GetObservation&service=SOS&version=1.0.0&observedProperty=',observedProperty,'&offering=REPLACE_SITE',sep="")
  supportedStats=getSupportedStatNames()
  tempArrays<-getEmptyResultArrayNWCStats(stats, length(sites), supportedStats)
  for (i in 1:length(sites)) {
    site = sites[i]
    sos_url<-gsub('REPLACE_SITE',site,sos)
    x_data <- SWE_CSV_IHA(sos_url)
    drain_area<-getWFSFieldAsNumeric(wfs_url,wfsTypename,wfsProperty,site,wfsPropertyname)
    tempArrays<-runStatsGroups(x_data,tempArrays,i,drain_area)
  }
  statsout<-nameStatsArray(stats, sites, tempArrays)
  return(statsout)
}