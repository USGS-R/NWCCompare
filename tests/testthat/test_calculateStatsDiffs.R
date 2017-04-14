context("calculateStatsDiffs")

test_that("Original demo for for diff stats works", {
  model_urls="https://cida.usgs.gov/nwc/thredds/sos/watersmart/HUC12_data/HUC12_Q.nc?request=GetObservation&service=SOS&version=1.0.0&observedProperty=MEAN_streamflow&offering=031601020108"
  startdate <- "1980-10-01"
  enddate <- "2010-09-30"
  nwisDvUrl <- "https://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
  sites <- "02435020"
  sites<-read.csv(header=F,colClasses=c("character"),text=sites)
  sites <- unlist(sites[1,])
  offering <- "00003"
  property <- "00060"
  x_urls<-paste0(nwisDvUrl, sites, "&startDT=", startdate, "&endDT=", enddate, "&statCd=", offering, "&parameterCd=", property)
  drainage_url <- "https://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
  d_urls<-paste0(drainage_url, sites)
  m_urls <- read.csv(header=F,colClasses=c("character"),text=model_urls)
  m_urls <- unlist(m_urls[1,])
  statsout <- calculateStatsDiffs(sites, startdate, enddate, EflowStats::getXMLWML1.1Data, x_urls, EflowStats::getDrainageArea, sites, SWE_CSV_IHA, m_urls)
  statsout_check <- readRDS("data/test_calculateStatsDiffs_huc_nwis_statsout.rds")
  expect_equal(statsout, statsout_check)
})