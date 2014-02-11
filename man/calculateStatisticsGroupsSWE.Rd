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
  \item{observedProperty}{Observed property to be queried from the SOS}
  \item{wfsUrl}{Base url to be queried for drainage area}
  \item{wfsTypename}{Type/layer name to be queried for drainage area}
  \item{wfsFilterProperty}{WFS attribute to be filtered on, should be the id of the attribute that cooresponds to site in the SOS}
  \item{wfsAreaPropertyname}{WFS attribute that contains drainage area.}
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
statsout <- calculateStatsGroupsSWE(stats, sites, sos, startdate, enddate, observedProperty, wfsUrl, wfsTypename, wfsFilterProperty,wfsAreaPropertyname)
}