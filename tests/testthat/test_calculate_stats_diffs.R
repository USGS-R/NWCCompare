context("calculate_stats_diffs")

test_that("Original demo for for diff stats works", {
  # Cooresponds to this page: https://cida.usgs.gov/nwc/#!waterbudget/achuc/031601020108
  hucs=c("031601020108")
  nwis_sites <- c("02435020")
  sites <- data.frame(a = nwis_sites, b = hucs)
  startdate <- "1980-10-01"
  enddate <- "2003-09-30" # old end date was 2010-09-30 but NWIS not available after 2004.
  flow_data_a <- build_nwis_dv_dataset(nwis_sites, 
                                       start_date = startdate, 
                                       end_date = enddate)
  flow_data_b <- build_nwc_flow_dataset(hucs, 
                                        start_date = startdate, 
                                        end_date = enddate)
  diff_statsout <- calculate_stats_diffs(sites = sites, 
                                    flow_data_a = flow_data_a, 
                                    flow_data_b = flow_data_b, 
                                    yearType = "water", 
                                    digits = 2)  
  statsout_check <- readRDS("data/test_calculate_stats_diffs_huc_nwis_statsout.rds")
  expect_equal(diff_statsout, statsout_check)
})