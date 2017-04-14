context("calculateStatsGroups")

test_that("Original demo for hucs works", {
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
statsout_check <- readRDS("data/test_calculateStatsGroups_statsout.rds")
expect_equal(statsout, statsout_check)
})

test_that("Original demo for NWIS works", { 
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
  statsout_check <- readRDS("data/test_calculateStatsGroups_nwis_statsout.rds")
  expect_equal(statsout, statsout_check)
})