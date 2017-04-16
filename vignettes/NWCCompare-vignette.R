## ----workflow, echo=TRUE, eval=TRUE--------------------------------------
library(EflowStats)
library(NWCCompare)

## ----modelStatsprep, echo=TRUE, eval=TRUE--------------------------------
# Run stats and differences on USGS observed and modeled daily discharge data
model_urls="https://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc?request=GetObservation&service=SOS&version=1.0.0&observedProperty=MEAN_streamflow&offering=031601020108"
startdate <- "1980-10-01"
enddate <- "2010-09-30"
nwisDvUrl <- "https://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
sites <- "02435020"
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
sites <- unlist(sites[1,])
offering <- "00003"
property <- "00060"

## ----modelStatschunk, echo=FALSE, eval=TRUE------------------------------
x_urls<-paste0(nwisDvUrl, sites, "&startDT=", startdate, "&endDT=", enddate, "&statCd=", offering, "&parameterCd=", property)
drainage_url <- "https://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
d_urls<-paste0(drainage_url, sites)
m_urls <- read.csv(header=F,colClasses=c("character"),text=model_urls)
m_urls <- unlist(m_urls[1,])

## ----createstatsoutput, echo=FALSE, eval=TRUE, results="hidew"-----------
# calculate statsout
statsout <- calculateStatsDiffs(sites, startdate, enddate, getXMLWML1.1Data, x_urls, getDrainageArea, sites, SWE_CSV_IHA, m_urls)

## ----statsoutput, echo=TRUE, eval=FALSE----------------------------------
#  # calculate statsout
#  statsout <- calculateStatsDiffs(sites, startdate, enddate, getXMLWML1.1Data, x_urls, getDrainageArea, sites, SWE_CSV_IHA, m_urls)

## ----viewData, echo=FALSE, eval=TRUE-------------------------------------
# view a portion of the statsout table
statsout[,c(1,4,39,74,109,111,115)]

## ----saveData, echo=TRUE, eval=FALSE-------------------------------------
#  # save statsout to a tab-delimited file
#  output = "output.txt"
#  write.table(statsout, file = output, col.names = TRUE, row.names = FALSE,
#              quote = FALSE, sep = "\t")

## ----OtherStats, echo=TRUE, eval=FALSE-----------------------------------
#  # calculate stats for data from your own data file
#  drain_area=54
#  site_id="Test site"
#  daily_data<-dailyData
#  stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
#  statsout <- ObservedStatsOther(daily_data,drain_area,site_id,stats)

## ----justStats, echo=TRUE, eval=FALSE------------------------------------
#  # Run stats on USGS observed daily discharge data
#  sites <- '02177000,02178400'
#  startdate <- "2008-10-01"
#  enddate <- "2013-09-29"
#  stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat,otherStat"
#  nwisDvUrl <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
#  offering <- "00003"
#  property <- "00060"
#  drainage_url <- "http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
#  sites<-read.csv(header=F,colClasses=c("character"),text=sites)
#  x_urls<-paste(nwisDvUrl, sites, "&startDT=", startdate, "&endDT=", enddate, "&statCd=", offering, "&parameterCd=", property, sep = "")
#  statsout <- calculateStatsGroups(stats, sites, startdate, enddate, getXMLWML1.1Data, x_urls, getDrainageArea, sites)

