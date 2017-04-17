context("get_local_data")

test_that("example for characterization", {
dataPath <- system.file("extdata", package="NWCCompare")
dataPath <- paste(dataPath, "modeled", sep="/")
startdate <- "2009"
enddate <- "2013"
localData <- get_local_data(dataPath,startDt=startdate,endDt=enddate)
localData_check <- readRDS("data/test_get_local_data.rds")
expect_equal(localData, localData_check)
})