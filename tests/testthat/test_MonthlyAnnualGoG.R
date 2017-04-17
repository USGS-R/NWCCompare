context("calculate_GoF_summary_stats")

test_that("example for characterization", {
  Gaged<-sampleData
  Modeled<-sampleData
  statsout <- calculate_GoF_summary_stats(Gaged,Modeled)
  statsout_check <- readRDS("data/test_calculate_GoF_summary_stats.rds")
  expect_equal(statsout, statsout_check)
})