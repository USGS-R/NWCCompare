context("build_nwis_dv_dataset")

test_that("nwis download works", {
  sites <- c("02177000","02178400")
  start_date <- "2008-10-01"
  end_date <- "2010-09-30"
  dataout <- build_nwis_dv_dataset(sites, start_date, end_date)
  dataout_check <- readRDS("data/test_build_nwis_dv_dataset.rds")
  expect_equal(dataout, dataout_check)
})

test_that("huc discharge works", {
  hucs <- c("031601020108","031501100104")
  start_date <- "1980-10-01"
  end_date <- "2010-09-30"
  dataout <- build_nwc_flow_dataset(hucs, start_date, end_date)
  dataout_check <- readRDS("data/test_build_nwc_flow_dataset.rds")
  expect_equal(dataout, dataout_check)
})

test_that("huc discharge with dates", {
  dataout <- build_nwc_flow_dataset(huc = "031300011004", 
                         start_date = "2004-10-01", 
                         end_date = "2010-09-30")
  dataout_check <- readRDS("data/test_build_nwc_flow_dataset_dates.rds")
  expect_equal(dataout, dataout_check)
})