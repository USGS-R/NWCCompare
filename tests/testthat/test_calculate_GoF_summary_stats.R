context("calculate_GoF_summary_stats")

test_that("example for characterization", {
  Gaged <- obs_data
  Gaged$date <- as.Date(Gaged$date)
  Gaged <- dataCheck(Gaged, yearType = "water")
  Modeled<-mod_data
  Modeled$date <- as.Date(Modeled$date)
  Modeled <- dataCheck(Modeled, yearType = "water")
  statsout <- calculate_GoF_summary_stats(Gaged,Modeled)
  statsout_check <- readRDS("data/test_calculate_GoF_summary_stats.rds")
  expect_equal(statsout, statsout_check)
})