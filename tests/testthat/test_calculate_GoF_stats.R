context("calculate_GoF_stats")
library(EflowStats)

test_that("test example returns the same", {
Gaged <- obs_data
Gaged$date <- as.Date(Gaged$date)
Gaged <- validate_data(Gaged, yearType = "water")
Modeled<-mod_data
Modeled$date <- as.Date(Modeled$date)
Modeled <- validate_data(Modeled, yearType = "water")
GoFstats <- calculate_GoF_stats(Modeled,Gaged)
Gofstats_check <- readRDS("data/test_calculate_GoF_stats.rds")
expect_equal(GoFstats, Gofstats_check)
})