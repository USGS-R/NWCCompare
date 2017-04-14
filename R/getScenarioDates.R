#' Function to return NWC Intercomparison portal start and end dates for a given model
#' 
#' This function accepts a url and returns a start and end date for modeled data
#' 
#' @param scenario_url url for SOS service for desired model data
#' @return getdate list of start and end dates for the chosen model
#' @export
#' @examples
#' scenario_url <- paste0("http://cida.usgs.gov/nwc/thredds/sos/watersmart/stats/",
#' "stats-SE-DENSE1-2.03.nc?request=GetCapabilities&service=SOS&version=1.0.0")
#' getScenarioDates(scenario_url)
getScenarioDates <- function(scenario_url){
  cat(paste("Retrieving start and end dates in scenario from: \n", scenario_url, "\n", sep=" "))
  doc<-xmlTreeParse(scenario_url, getDTD=F, useInternalNodes=TRUE)
  startDate <- xpathSApply(doc,"//Capabilities/Contents/ObservationOfferingList/ObservationOffering/time/gml:TimePeriod/gml:beginPosition",xmlValue)[[1]]
  endDate <- xpathSApply(doc,"//Capabilities/Contents/ObservationOfferingList/ObservationOffering/time/gml:TimePeriod/gml:endPosition",xmlValue)[[1]]
  scenario_dates<-c(substr(startDate,1,10),substr(endDate,1,10))
  getdate<-scenario_dates
  return (getdate)
}