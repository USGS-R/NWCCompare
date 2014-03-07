\name{getEmptyResultArrayNWCStats}
\alias{getEmptyResultArrayNWCStats}
\title{Get Empty Result Arrays}
\description{Convenience function to get a list of arrays to put results into}
\usage{
getEmptyResultArrayNWCStats(stats, numSites, supportedStats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stats}{Comma seperated string of stats like: "magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"}
  \item{numSites}{Number of sites (rows)}
  \item{supportedStats}{Response from getSupportedStatNames() function}
  }
\value{An empty set of named lists.}
\references{
%% ~put references to the literature/web site here ~
}
\author{David Blodgett dblodgett@usgs.gov}
\examples{
emptyArrays<-getEmptyResultArrayNWCStats("GoF", 2, getSupportedStatNames())
}