context("Test SWE CSV Data Parses")

test_that("Test that the function runs without errors", {
  outData<-parse_swe_csv(system.file('extdata','SWECSVBlock_daymet_example.xml',package='NWCCompare'))
  expect_equal(length(outData),2)
  expect_equal(length(outData$data),12784)
  expect_equal(round(mean(outData$data),digits = 4) ,3.5134)
})

test_that("xml exception is handled.", {
  expect_error(parse_swe_csv(system.file('extdata','exception.xml',package='NWCCompare')),
               'An invalid parameter error was encountered. The HUC may not exist.')
})
