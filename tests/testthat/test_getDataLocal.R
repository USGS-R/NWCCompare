context("getDataLocal")

test_that("example for characterization", {
dataPath <- system.file("extdata", package="NWCCompare")
dataPath <- paste(dataPath, "modeled", sep="/")
startdate <- "2009"
enddate <- "2013"
localData <- getDataLocal(dataPath,startDt=startdate,endDt=enddate)
localData_check <- readRDS("data/test_getDataLocal.rds")
expect_equal(localData, localData_check)
})