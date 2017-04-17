context("get_obsdata")

test_that("nwis from dataRetrieval", {
  sites<-"02177000"
  startdate<-"2011-10-01"
  enddate<-"2012-09-30"
  x_obs <- dataRetrieval::readNWISdv(siteNumber = sites,
                                     parameterCd = "00060",
                                     startDate = startdate,
                                     endDate = enddate)
  obs_data <- NWCCompare::get_obsdata(x_obs)
  expect_equal(obs_data, readRDS("data/test_getobsdata_nwis.rds"))
})

test_that("hucs from get_nwc_wb_data", {
  huc="031601020108"
  startdate <- "1980-10-01"
  enddate <- "2010-09-30"
  x_obs <- get_nwc_wb_data(huc)
  x_obs <- x_obs$discharge
  colnames(x_obs)<-c("date","discharge")
  obs_data <- NWCCompare::get_obsdata(x_obs)
  expect_equal(obs_data, readRDS("data/test_getobsdata_huc.rds"))
})