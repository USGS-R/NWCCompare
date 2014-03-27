\name{MonthlyAnnualGoF}
\alias{MonthlyAnnualGoF}
\title{Function to calculate monthly annual GoF statistics for given observed and modeled data sets}
\usage{
MonthlyAnnualGoF(Gaged, Modeled)
}
\arguments{
  \item{Gaged}{data frame of daily flow data}

  \item{Modeled}{data frame of daily flow data}
}
\value{
Output data frame of calculated statistics
}
\description{
This function accepts observed and modeled data frames of
daily flow data and returns a data frame of calculated
monthly annual GoF statistics
}
\examples{
Gaged<-qfiletempf
Modeled<-qfiletempf
MonthlyAnnualGoF(Gaged,Modeled)
}

