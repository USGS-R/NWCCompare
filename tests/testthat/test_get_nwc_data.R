context("Test Data Downloads")

test_that("Test that the function returns the expected number of time series", {
  outData<-get_nwc_wb_data("031601030306")
  expect_equal(length(names(outData)),3)
  expect_true(length(outData$discharge)>0)
  outData<-get_nwc_wb_data("031601030306", local=TRUE)
  expect_equal(length(names(outData)),2)
  outData<-get_nwc_wb_data("03160103", local=TRUE)
  expect_equal(length(names(outData)),2)
  expect_error(get_nwc_wb_data("03160103", local=FALSE),
               "Total upstream HUC08 watersheds are not available yet.")
  outData<-get_nwc_wb_data("031601030306", return_var = "prcp")
  expect_equal(names(outData), "prcp")
})

test_that("Test that a HUC with NWIS data comes back as such.",{
  outData<-get_nwc_wb_data("121101110705", local=FALSE)
  expect_equal(length(names(outData$streamflow)),5)
})

test_that('we get back some geojson when calling the getNWCWatershed function',{
  outData<-get_nwc_huc("031601030306",local=TRUE)
  expect_gt(length(outData$features[[1]]$geometry$coordinates[[1]][[1]]),100)
  expect_equal(outData$type,"FeatureCollection")
})
