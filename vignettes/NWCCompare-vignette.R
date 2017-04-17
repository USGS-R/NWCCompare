## ----workflow, echo=TRUE, eval=TRUE--------------------------------------
library(EflowStats)
library(NWCCompare)

## ----modelStatsprep, echo=TRUE, eval=TRUE--------------------------------
# Run stats and differences on USGS observed and modeled daily discharge data
hucs="031601020108"
startdate <- "1980-10-01"
enddate <- "2010-09-30"
sites <- "02435020"
sites<-read.csv(header=F,colClasses=c("character"),text=sites)
sites <- unlist(sites[1,])

## ----modelStatschunk, echo=FALSE, eval=TRUE------------------------------
drainage_url <- "https://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
d_urls<-paste0(drainage_url, sites)
hucs <- read.csv(header=F,colClasses=c("character"),text=hucs)
hucs <- unlist(hucs[1,])

## ----createstatsoutput, echo=FALSE, eval=TRUE, results="hidew"-----------
# calculate statsout
statsout <- calculateStatsDiffs(sites = sites, 
                                startdate = startdate, 
                                enddate = enddate, 
                                X_DATA_FUN = dataRetrieval::readNWISdv, 
                                x_args = sites, 
                                DRAIN_AREA_FUN = getDrainageArea, 
                                drain_args = sites, 
                                M_DATA_FUN = get_nwc_wb_data, 
                                m_args = hucs)  

## ----statsoutput, echo=TRUE, eval=FALSE----------------------------------
#  # calculate statsout
#  statsout <- calculateStatsDiffs(sites = sites,
#                                  startdate = startdate,
#                                  enddate = enddate,
#                                  X_DATA_FUN = dataRetrieval::readNWISdv,
#                                  x_args = sites,
#                                  DRAIN_AREA_FUN = getDrainageArea,
#                                  drain_args = sites,
#                                  M_DATA_FUN = get_nwc_wb_data,
#                                  m_args = hucs)

## ----viewData, echo=FALSE, eval=TRUE-------------------------------------
# view a portion of the statsout table
statsout[,c(1,4,39,74,109,111,115)]

## ----saveData, echo=TRUE, eval=FALSE-------------------------------------
#  # save statsout to a tab-delimited file
#  output = "output.txt"
#  write.table(statsout, file = output, col.names = TRUE, row.names = FALSE,
#              quote = FALSE, sep = "\t")

## ----OtherStats, echo=TRUE, eval=FALSE-----------------------------------
#  # calculate stats for data from your own data file
#  drain_area=54
#  site_id="Test site"
#  daily_data<-dailyData
#  stats="magnifSeven,magStat,flowStat,durStat,timStat,rateStat,otherStat"
#  statsout <- ObservedStatsOther(daily_data,drain_area,site_id,stats)

## ----justStats, echo=TRUE, eval=FALSE------------------------------------
#  # Run stats on USGS observed daily discharge data
#  sites <- '02177000,02178400'
#  startdate <- "2008-10-01"
#  enddate <- "2013-09-29"
#  stats<-"rateStat,magnifSeven,magStat,flowStat,durStat,timStat,otherStat"
#  drainage_url <- "http://waterservices.usgs.gov/nwis/site/?siteOutput=Expanded&site="
#  sites<-read.csv(header=F,colClasses=c("character"),text=sites)
#  statsout <- calculateStatsGroups(stats = stats,
#                                   sites = sites,
#                                   startdate = startdate,
#                                   enddate = enddate,
#                                   X_DATA_FUN = dataRetrieval::readNWISdv,
#                                   x_args = sites,
#                                   DRAIN_AREA_FUN = getDrainageArea,
#                                   drain_args = sites)

