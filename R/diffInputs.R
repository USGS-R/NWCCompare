#' Function to return the inputs for the calculate stats diff function
#' 
#' @param stats string of requested stats
#' @param model_url url for access to modeled discharge data
#' @return diffInputs data frame of calculated statistics
#' @export
diffInputs<-function(stats, model_url)  {
  
  nwisDvUrl <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
  offering <- "00003"
  property <- "00060"
  drainage_url <- "http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
  scenario_url <- paste(substr(model_url,1,regexpr("Get",model_url)-1),"GetCapabilities&service=SOS&version=1.0.0",sep="")
  
  getcap<-getScenarioSites(scenario_url)
  modprop<-getcap$modprop
  sites<-getcap$scenario_sites
  sites <- paste(sites,collapse=",")
  
  sites<-read.csv(header=F,colClasses=c("character"),text=sites)
  dates <- getScenarioDates(scenario_url)
  startdate <- dates[1]
  enddate <- dates[2]
  
  x_urls<-paste(nwisDvUrl, sites, "&startDT=", startdate, "&endDT=", enddate, "&statCd=", offering, "&parameterCd=", property, sep = "")
  d_urls<-paste(drainage_url, sites, sep = "")
  m_urls<-paste(model_url,'=',sites,'&observedProperty=',modprop,sep='',collapse=NULL)
  diffInputs <- list(startdate,enddate,x_urls,d_urls,m_urls,sites)
  return(diffInputs)
}
