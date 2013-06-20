\name{rsr}
\alias{rsr}
\title{Function to return the ratio of the root mean square error to the standard deviation}
\usage{
  rsr(timeseries1, timeseries2)
}
\arguments{
  \item{timeseries1}{data frame containing value data for
  the observed timeseries}

  \item{timeseries2}{data frame containing value data for
  the modeled timeseries}
}
\value{
  rsr root mean square error/standard deviation for the two
  timeseries
}
\description{
  This function accepts observed and modeled daily data
  series and returns the root mean square error/standard
  deviation
}
\examples{
obs_data<-paste(system.file(package="NWCCompare"),"/data/obs_data.csv",sep="")
mod_data<-paste(system.file(package="NWCCompare"),"/data/mod_data.csv",sep="")
timeseries1<-read.csv(obs_data)$discharge
timeseries2<-read.csv(mod_data)$discharge
rsr(timeseries1,timeseries2)
}

