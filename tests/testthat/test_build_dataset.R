context("calculate_stats_by_group")

test_that("Original demo for hucs works", {
  sites <- c("02177000","02178400")
  start_date <- "2008-10-01"
  end_date <- "2010-09-30"
  dataout <- build_nwis_dv_dataset(sites, start_date, end_date)
  dataout_check <- readRDS("data/test_build_nwis_dv_dataset.rds")
  expect_equal(dataout, dataout_check)
})

test_that("Original demo for hucs works", {
  hucs <- c("031601020108","031501100104")
  start_date <- "2008-10-01"
  end_date <- "2010-09-30"
  dataout <- build_nwc_flow_dataset(hucs, start_date, end_date)
  dataout_check <- readRDS("data/test_build_nwc_flow_dataset.rds")
  expect_equal(dataout, dataout_check)
})