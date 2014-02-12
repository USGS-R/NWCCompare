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
      drain_area<-DRAIN_AREA_FUN(drain_args[1])
    }
    tempArrays<-runStatsGroups(x_data,tempArrays,i,drain_area)
  }
  statsout<-nameStatsArray(stats, sites, tempArrays)
  return(statsout)
}