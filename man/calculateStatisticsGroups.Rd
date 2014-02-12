\name{calculateStatsGroupsSWE}
\alias{calculateStatsGroupsSWE}
\title{Calculate Statistics for SWE CSV SOS Streamflow Sites by Statistic Group}
\description{Takes a list of stats groups an nwis site and a date range. Returns a dataframe with the requested statistics.}
\usage{calculateStatsGroupsSWE(stats, sites, sos, startdate, enddate, observedProperty, wfsUrl, wfsTypename, wfsFilterProperty,wfsAreaPropertyname)}
\arguments{
  \item{stats}{Comma seperated string of stats like: "magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"}
  \item{sites}{result of read.csv from a comma sperated list}
  \item{startdate}{Like: "2008-10-01"}
  \item{enddate}{Like: "2013-09-30"}
  X_DATA_FUN, , DRAIN_AREA_FUN, drain_args, drain_site_param=NULL
  \item{X_DATA_FUN}{The function to get flow data}
  \item{x_args}{A vector of arguments (constructed urls) for the flow data function}
  \item{DRAIN_AREA_FUN}{The function to get drainage area}
  \item{drain_args}{Arguments to pass to the drainage area function, can be a vector of urls or a named list of arguments. If a named list is used, the drain_site_param is required.}
  \item{drain_site_param}{Parameter name to assign the site ids to for use with the drainage area function}
}
\value{Data frame of calculated stats with headers.}
\author{Dave Blodgett: dblodgett@usgs.gov}

\examples{
sites<-"031401020800,031401020800"
startdate <- "2008-10-01"
enddate <- "2010-09-29"
stats<-"magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
sos<-"http://cida-wiwsc-wsqa.er.usgs.gov:8081/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc" 
observedProperty="MEAN_streamflow"
wfsUrl<-'http://cida.usgs.gov/nwc/geoserver/NHDPlusHUCs/ows'
wfsTypename='NHDPlusHUCs:huc12_SE_Basins_v2'
wfsFilterProperty='NHDPlusHUCs:HUC12'
wfsAreaPropertyname='NHDPlusHUCs:mi2'
stats<-"magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
urls<-paste(sos,'?request=GetObservation&service=SOS&version=1.0.0&observedProperty=',observedProperty,'&offering=',sites,sep="")
statsout <- calculateStatsGroups(stats, sites, startdate, enddate, SWE_CSV_IHA, urls, getWFSFieldAsNumeric, drain_args=list(wfs_url=wfsUrl, wfsTypename=wfsTypename, wfsProperty=wfsFilterProperty, wfsPropertyname=wfsAreaPropertyname), drain_site_param='wfsLiteral')
}