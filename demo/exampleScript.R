
install.packages(c("zoo","chron","doBy","XML","hydroGOF","lmomco","RCurl"))
install.packages(c("EflowStats","NWCCompare"),repos="http://usgs-r.github.com",type="source")

library(EflowStats)
library(NWCCompare)

# Run stats and differences on USGS observed and modeled daily discharge data
# Cooresponds to this page: https://cida.usgs.gov/nwc/#!waterbudget/achuc/031601020108
model_urls="https://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc?request=GetObservation&service=SOS&version=1.0.0&observedProperty=MEAN_streamflow&offering=031601020108"
startdate <- "1980-10-01"
enddate <- "2010-09-30"
nwisDvUrl <- "https://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
sites <- "02435020"
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
sites <- unlist(sites[1,])
offering <- "00003"
property <- "00060"
x_urls<-paste0(nwisDvUrl, sites, "&startDT=", startdate, "&endDT=", enddate, "&statCd=", offering, "&parameterCd=", property)
drainage_url <- "https://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
d_urls<-paste0(drainage_url, sites)
m_urls <- read.csv(header=F,colClasses=c("character"),text=model_urls)
m_urls <- unlist(m_urls[1,])
statsout <- calculateStatsDiffs(sites, startdate, enddate, EflowStats::getXMLWML1.1Data, x_urls, EflowStats::getDrainageArea, sites, SWE_CSV_IHA, m_urls)

# Run stats on modeled huc12s
sites<-"031601020108"
startdate <- "2008-10-01"
enddate <- "2010-09-29"
stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat"
sos<-"https://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc"
observedProperty="MEAN_streamflow"
wfsUrl<-'https://cida.usgs.gov/nwc/geoserver/ows'
wfsTypename='WBD:huc12'
wfsFilterProperty='WBD:huc12'
wfsAreaPropertyname='WBD:areasqkm' ## Note units are wrong now!
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
urls<-paste(sos,'?request=GetObservation&service=SOS&version=1.0.0&observedProperty=',observedProperty,'&offering=',sites,sep="")
statsout <- calculateStatsGroups(stats, sites, startdate, enddate, SWE_CSV_IHA, urls, getWFSFieldAsNumeric, drain_args=list(wfs_url=wfsUrl, wfsTypename=wfsTypename, wfsProperty=wfsFilterProperty, wfsPropertyname=wfsAreaPropertyname), drain_site_param='wfsLiteral')

# Run stats on USGS observed daily discharge data
sites <- '02177000,02178400'
startdate <- "2008-10-01"
enddate <- "2013-09-29"
stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat,otherStat"
nwisDvUrl <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering <- "00003"
property <- "00060"
drainage_url <- "http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
x_urls<-paste(nwisDvUrl, sites, "&startDT=", startdate, "&endDT=", enddate, "&statCd=", offering, "&parameterCd=", property, sep = "")
statsout <- calculateStatsGroups(stats, sites, startdate, enddate, EflowStats::getXMLWML1.1Data, x_urls, EflowStats::getDrainageArea, sites)  
