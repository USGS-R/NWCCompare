\name{nameStatsArray}
\alias{nameStatsArray}
\title{Apply names to stats array}
\description{convenience function.}
\usage{nameStatsArray(stats, sites, tempArrays)}
\arguments{
  \item{stats}{Comma seperated string of stats like: "magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"}
  \item{sites}{result of read.csv from a comma sperated list.}
  \item{tempArrays}{arrays as returned by getEmptyResultArrayNWCStats}
}
\value{The temp arrays turned into a single response array.}
\author{David Blodgett dblodgett@usgs.gov}
\examples{
stats<-"magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
sites<-"031401020800,031401020800"
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
tempArrays<-getEmptyResultArrayNWCStats(stats, 1, getSupportedStatNames())
nameStatsArray(stats, sites, tempArrays)
}
