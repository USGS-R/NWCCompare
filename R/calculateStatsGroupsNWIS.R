calculateStatsGroupsNWIS<-function(stats, sites, startdate, enddate) {
  # Hardcode NWIS urls and parameters.
  nwisDvUrl <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
  offering <- "00003"
  property <- "00060"
  drainage_url <- "http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
  supportedStats=getSupportedStatNames()
  tempArrays<-getEmptyResultArrayNWCStats(stats, length(sites), supportedStats)
  for (i in 1:length(sites)) {
    site = sites[i]
    x_data <- getXMLWML1.1Data(paste(nwisDvUrl, site, "&startDT=", startdate, "&endDT=", enddate, "&statCd=", offering, "&parameterCd=", property, sep = ""))
    drain_area <- getDrainageArea(paste(drainage_url, site, sep = ""))
    tempArrays<-runStatsGroups(x_data,tempArrays,i,drain_area)
  }
  statsout<-nameStatsArray(stats, sites, tempArrays)
  return(statsout)
}