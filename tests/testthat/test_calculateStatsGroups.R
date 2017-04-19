context("calculate_stats_by_group")

test_that("Original demo for hucs works", {
  sites<-"031601020108"
  startdate <- "2008-10-01"
  enddate <- "2010-09-29"
  stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat"
  sites<-read.csv(header=F,colClasses=c("character"),text=sites)
  statsout <- calculate_stats_by_group(stats = stats, 
                                   sites = sites, 
                                   startdate = startdate, 
                                   enddate = enddate, 
                                   X_DATA_FUN = get_nwc_wb_data, 
                                   x_args = sites, 
                                   DRAIN_AREA_FUN = get_nwc_huc, 
                                   drain_args=sites)
  statsout_check <- readRDS("data/test_calculate_stats_by_group_statsout.rds")
  expect_equal(statsout, statsout_check)
})

test_that("Original demo for NWIS works", { 
  sites <- c("02177000","02178400")
  startdate <- "2008-10-01"
  enddate <- "2013-09-30"
  nwis_dataset <- build_nwis_dv_dataset(sites, startdate, enddate)
  stats=c("magAverage", "magLow", "magHigh",
          "frequencyLow", "frequencyHigh",
          "durationLow", "durationHigh",
          "timingAverage", "timingLow", "timingHigh",
          "rateChange",
          "magnifSeven", "otherStat")
  statsout <- calculate_stats_by_group(stats = stats, 
                                       flow_data = nwis_dataset, 
                                       yearType = "water",
                                       digits = 2)  
  statsout_check <- readRDS("data/test_calculate_stats_by_group_nwis_statsout.rds")
  expect_equal(statsout, statsout_check)
})