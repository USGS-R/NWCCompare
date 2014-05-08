\name{getWFSFieldAsNumeric}
\alias{getWFSFieldAsNumeric}
\title{Function to return the drainage area as a numeric}
\usage{
getWFSFieldAsNumeric(wfs_url, wfsTypename, wfsProperty, wfsLiteral,
  wfsPropertyname)
}
\arguments{
  \item{wfs_url}{url of WFS}

  \item{wfsTypename}{type to paste into url}

  \item{wfsProperty}{property to paste into url}

  \item{wfsLiteral}{site_no to paste into url}

  \item{wfsPropertyname}{property name to paste into url}
}
\value{
drain_area numeric of site drainage area
}
\description{
Function to return the drainage area as a numeric
}
\examples{
\dontrun{
wfs_url<-'http://cida.usgs.gov/nwc/geoserver/NHDPlusHUCs/ows'
wfsTypename='NHDPlusHUCs:huc12_SE_Basins_v2'
wfsProperty='NHDPlusHUCs:HUC12'
wfsPropertyname='NHDPlusHUCs:mi2'
wfsLiteral='031401020800'
area<-getWFSFieldAsNumeric(wfs_url,wfsTypename,wfsProperty,wfsLiteral,wfsPropertyname)
}
}

