\name{getWFSFieldAsNumeric}
\alias{getWFSFieldAsNumeric}
\title{Get WFS field as a Number}
\description{Get WFS field as a Number}
\usage{getWFSFieldAsNumeric(wfsUrl,wfsTypename,wfsFilterProperty,wfsAreaPropertyname,wfsLiteral,,wfsAreaPropertyname)}
\arguments{
  \item{wfsUrl}{Base url to be queried for}
  \item{wfsTypename}{Type/layer name to be queried}
  \item{wfsFilterProperty}{WFS attribute to be filtered on.}
  \item{wfsAreaPropertyname}{WFS attribute that contains the number to be returned.}
  }

\value{A number extracted from the WFS.}
\author{David Blodgett dblodgett@usgs.gov}
\examples{
wfsUrl<-'http://cida.usgs.gov/nwc/geoserver/NHDPlusHUCs/ows'
wfsTypename='NHDPlusHUCs:huc12_SE_Basins_v2'
wfsFilterProperty='NHDPlusHUCs:HUC12'
wfsAreaPropertyname='NHDPlusHUCs:mi2'
wfsLiteral='031401020800'
area<-getWFSFieldAsNumeric(wfsUrl,wfsTypename,wfsFilterProperty,wfsLiteral,wfsAreaPropertyname)
}