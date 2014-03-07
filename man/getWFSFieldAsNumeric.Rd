\name{getWFSFieldAsNumeric}
\alias{getWFSFieldAsNumeric}
\title{Get WFS field as a Number}
\description{Get WFS field as a Number}
\usage{getWFSFieldAsNumeric(wfs_url,wfsTypename,wfsProperty,wfsLiteral,wfsPropertyname)}
\arguments{
  \item{wfs_url}{Base url to be queried for}
  \item{wfsTypename}{Type/layer name to be queried}
  \item{wfsProperty}{WFS attribute to be filtered on.}
  \item{wfsLiteral}{field holding site numter}
  \item{wfsPropertyname}{WFS attribute that contains the number to be returned.}
  }

\value{A number extracted from the WFS.}
\author{David Blodgett dblodgett@usgs.gov}
\examples{
wfs_url<-'http://cida.usgs.gov/nwc/geoserver/NHDPlusHUCs/ows'
wfsTypename='NHDPlusHUCs:huc12_SE_Basins_v2'
wfsProperty='NHDPlusHUCs:HUC12'
wfsPropertyname='NHDPlusHUCs:mi2'
wfsLiteral='031401020800'
area<-getWFSFieldAsNumeric(wfsUrl,wfsTypename,wfsFilterProperty,wfsLiteral,wfsPropertyname)
}