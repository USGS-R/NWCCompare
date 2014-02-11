\name{runStatsGroups}
\alias{runStatsGroups}
\title{Run statistics by group}
\description{Convenience function to calculate statstics by group.}
\usage{
runStatsGroups(x_data,tempArrays,i,drain_area)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x_data}{Input data as returned SWE_CSV_IHA or getXMLWML1.1Data}
  \item{tempArrays}{arrays as returned by getEmptyResultArrayNWCStats}
  \item{i}{Element to access in tempArrays lists.}
  \item{drain_area}{The drainage area for the site in square miles}
}
\value{A list of arrays containing the results of the calculations.}
\author{David Blodgett dblodgett@usgs.gov}
\examples{
stats<-"magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
x_data <- getXMLWML1.1Data("http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites=02177000&startDT=2008-10-01&endDT=2010-09-29&statCd=00003&parameterCd=00060")
drain_area <- getDrainageArea("http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site=02177000")
tempArrays<-getEmptyResultArrayNWCStats(stats, 1, getSupportedStatNames())
i<-1
tempArrays<-runStatsGroups(x_data,tempArrays,i,drain_area)
}
