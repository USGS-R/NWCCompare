context("calculateStatsGroups")

test_that("Original demo for hucs works", {
  sites<-"031601020108"
  startdate <- "2008-10-01"
  enddate <- "2010-09-29"
  stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat"
  sites<-read.csv(header=F,colClasses=c("character"),text=sites)
  statsout <- calculateStatsGroups(stats = stats, 
                                   sites = sites, 
                                   startdate = startdate, 
                                   enddate = enddate, 
                                   X_DATA_FUN = get_nwc_wb_data, 
                                   x_args = sites, 
                                   DRAIN_AREA_FUN = get_nwc_huc, 
                                   drain_args=sites)
  statsout_check <- readRDS("data/test_calculateStatsGroups_statsout.rds")
  expect_equal(statsout, statsout_check)
})

test_that("Original demo for NWIS works", { 
  sites <- '02177000,02178400'
  startdate <- "2008-10-01"
  enddate <- "2013-09-29"
  stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat,otherStat"
  drainage_url <- "http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
  sites<-read.csv(header=F,colClasses=c("character"),text=sites)
  statsout <- calculateStatsGroups(stats = stats, 
                                   sites = sites, 
                                   startdate = startdate, 
                                   enddate = enddate, 
                                   X_DATA_FUN = dataRetrieval::readNWISdv, 
                                   x_args = sites, 
                                   DRAIN_AREA_FUN = dataRetrieval::readNWISsite, 
                                   drain_args = sites)  
  statsout_check <- readRDS("data/test_calculateStatsGroups_nwis_statsout.rds")
  expect_equal(statsout, statsout_check)
})