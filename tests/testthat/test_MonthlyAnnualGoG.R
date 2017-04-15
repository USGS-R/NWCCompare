context("MonthlyAnnualGoF")

test_that("example for characterization", {
  Gaged<-sampleData
  Modeled<-sampleData
  statsout <- MonthlyAnnualGoF(Gaged,Modeled)
  statsout_check <- readRDS("data/test_MonthlyAnnualGoF.rds")
  expect_equal(statsout, statsout_check)
})