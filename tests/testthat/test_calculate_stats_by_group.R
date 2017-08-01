context("calculate_stats_by_group")

# test_that("Original demo for hucs works", {
#   sites<-"031601020108"
#   startdate <- "2008-10-01"
#   enddate <- "2010-09-29"
#   stats<-"rateStat,calc_magnifSeven,magStat,flowStat,durStat,timStat"
#   sites<-read.csv(header=F,colClasses=c("character"),text=sites)
#   statsout <- calculate_stats_by_group(stats = stats, 
#                                    sites = sites, 
#                                    startdate = startdate, 
#                                    enddate = enddate, 
#                                    X_DATA_FUN = get_nwc_wb_data, 
#                                    x_args = sites, 
#                                    DRAIN_AREA_FUN = get_nwc_huc, 
#                                    drain_args=sites)
#   statsout_check <- readRDS("data/test_calculate_stats_by_group_statsout.rds")
#   expect_equal(statsout, statsout_check)
# })

test_that("Original demo for NWIS works", { 
  sites <- c("02177000","02178400")
  startdate <- "2008-10-01"
  enddate <- "2013-09-30"
  nwis_dataset <- build_nwis_dv_dataset(sites, startdate, enddate)
  
  stats=c("calc_magAverage", "calc_magLow", "calc_magHigh",
          "calc_frequencyLow", "calc_frequencyHigh",
          "calc_durationLow", "calc_durationHigh",
          "calc_timingAverage", "calc_timingLow", "calc_timingHigh",
          "calc_rateChange",
          "calc_magnifSeven", "otherStat")
  
  statsout <- calculate_stats_by_group(stats = stats, 
                                       flow_data = nwis_dataset, 
                                       yearType = "water",
                                       digits = 2)
  
  statsout_check <- readRDS("data/test_calculate_stats_by_group_nwis_statsout.rds")
  
  for(i in 1:length(names(statsout))) {
    expect_equal(names(statsout)[i], names(statsout_check)[i])
  }
  
  for(i in 4:(length(names(statsout))-1)) {
    if(!is.nan(statsout[1,i]) && !is.nan(statsout_check[1,i])) {
      abs_diff <- abs(statsout[1,i]-statsout_check[1,i])
      if(abs_diff > 0.01) {
        print(paste(names(statsout)[i], "difference is", abs_diff))
        expect_lt(abs(statsout[1,i]-statsout_check[1,i]), 0.1)
      }
    }
  }
  
  expect_equal(statsout, statsout_check)
})