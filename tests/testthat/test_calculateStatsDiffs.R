context("calculateStatsDiffs")

test_that("Original demo for for diff stats works", {
  # Cooresponds to this page: https://cida.usgs.gov/nwc/#!waterbudget/achuc/031601020108
  hucs="031601020108"
  startdate <- "1980-10-01"
  enddate <- "2010-09-30"
  nwisDvUrl <- "https://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
  sites <- "02435020"
  sites<-read.csv(header=F,colClasses=c("character"),text=sites)
  sites <- unlist(sites[1,])
  drainage_url <- "https://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
  d_urls<-paste0(drainage_url, sites)
  hucs <- read.csv(header=F,colClasses=c("character"),text=hucs)
  hucs <- unlist(hucs[1,])
  statsout <- calculateStatsDiffs(sites = sites, 
                                  startdate = startdate, 
                                  enddate = enddate, 
                                  X_DATA_FUN = dataRetrieval::readNWISdv, 
                                  x_args = sites, 
                                  DRAIN_AREA_FUN = dataRetrieval::readNWISsite, 
                                  drain_args = sites, 
                                  M_DATA_FUN = get_nwc_wb_data, 
                                  m_args = hucs)  
  statsout_check <- readRDS("data/test_calculateStatsDiffs_huc_nwis_statsout.rds")
  expect_equal(statsout, statsout_check)
})