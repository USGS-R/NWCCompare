\name{rmse}
\alias{rmse}
\title{Function to return the root mean square error between two data series}
\usage{
rmse(timeseries1, timeseries2)
}
\arguments{
  \item{timeseries1}{data frame containing value data for
  one of the chosen timeseries}

  \item{timeseries2}{data frame continaing value data for
  the second chosen timeseries}
}
\value{
rmse root mean square error value between the two
timeseries
}
\description{
This function accepts two data frames containing daily data
series and returns the root mean square error
}
\examples{
obs_data<-obs_data.RData
mod_data<-mod_data.RData
timeseries1<-obs_data$discharge
timeseries2<-mod_data$discharge
rmse(timeseries1,timeseries2)
}

