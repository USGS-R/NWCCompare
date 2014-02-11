\name{calculateStatsGroupsNWIS}
\alias{calculateStatsGroupsNWIS}
\title{Calculate Statistics for NWIS Streamflow Sites by Statistic Group}
\description{Takes a list of stats groups an nwis site and a date range. Returns a dataframe with the requested statistics.}
\usage{calculateStatsGroupsNWIS(stats, sites, startdate, enddate)}
\arguments{
  \item{stats}{Comma seperated string of stats like: "magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"}
  \item{sites}{result of read.csv from a comma sperated list.}
  \item{startdate}{Like: "2008-10-01"}
  \item{enddate}{Like: "2013-09-30"}
}
\value{Data frame of calculated stats with headers.}
\author{Dave Blodgett: dblodgett@usgs.gov}

\examples{
sites <- '02177000,02178400'
startdate <- "2008-10-01"
enddate <- "2013-09-29"
stats<-"magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
statsout <- calculateStatsGroupsNWIS(stats, sites, startdate, enddate)
}