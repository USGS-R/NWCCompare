
install.packages(c("zoo","chron","doBy","XML","hydroGOF","lmomco","RCurl"))
install.packages(c("EflowStats","NWCCompare"),repos="http://usgs-r.github.com",type="source")

library(EflowStats)
library(NWCCompare)

# Run stats and differences on USGS observed and modeled daily discharge data
stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat"
model_url="http://cida.usgs.gov/nwc/thredds/sos/watersmart/stats/stats-SE-DENSE2-2.03.nc?request=GetObservation&service=SOS&version=1.0.0&offering"
diffInputsv <- diffInputs(stats, model_url)
startdate <- diffInputsv[[1]]
enddate <- diffInputsv[[2]]
x_urls <- diffInputsv[[3]]
d_urls <- diffInputsv[[4]]
m_urls <- diffInputsv[[5]]
statsout <- calculateStatsDiffs(sites, startdate, enddate, getXMLWML1.1Data, x_urls, getDrainageArea, d_urls, SWE_CSV_IHA, m_urls)

# Run stats on modeled huc12s
sites<-"031401020800"
startdate <- "2008-10-01"
enddate <- "2010-09-29"
stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat"
sos<-"http://cida-eros-wsqa.er.usgs.gov:8081/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc"
observedProperty="MEAN_streamflow"
wfsUrl<-'http://cida-eros-wsdev.er.usgs.gov:8081/geoserver/NWC/ows'
wfsTypename='NWC:huc12_SE_Basins_v2'
wfsFilterProperty='NWC:HUC12'
wfsAreaPropertyname='NWC:mi2'
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
urls<-paste(sos,'?request=GetObservation&service=SOS&version=1.0.0&observedProperty=',observedProperty,'&offering=',sites,sep="")
statsout <- calculateStatsGroups(stats, sites, startdate, enddate, SWE_CSV_IHA, urls, getWFSFieldAsNumeric, drain_args=list(wfs_url=wfsUrl, wfsTypename=wfsTypename, wfsProperty=wfsFilterProperty, wfsPropertyname=wfsAreaPropertyname), drain_site_param='wfsLiteral')

# Run stats on USGS observed daily discharge data
sites <- '02177000,02178400'
startdate <- "2008-10-01"
enddate <- "2013-09-29"
stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat"
nwisDvUrl <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
offering <- "00003"
property <- "00060"
drainage_url <- "http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
x_urls<-paste(nwisDvUrl, sites, "&startDT=", startdate, "&endDT=", enddate, "&statCd=", offering, "&parameterCd=", property, sep = "")
d_urls<-paste(drainage_url, sites, sep = "")
statsout <- calculateStatsGroups(stats, sites, startdate, enddate, getXMLWML1.1Data, x_urls, getDrainageArea, d_urls)
