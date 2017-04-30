## ----loadWebServiceData, echo=TRUE, eval=TRUE----------------------------
library(NWCCompare)
# First we set out site ids.
nwis <- "02335757"
huc <- "031300011004"
# Then set our start and end dates. Note these define water years.
start_date <- "2004-10-01"
end_date <- "2010-09-30"
flow_data_nwis <- build_nwis_dv_dataset(nwis, start_date, end_date)
flow_data_nwc <- build_nwc_flow_dataset(huc, start_date, end_date)
str(flow_data_nwis)

## ----getLocalData, echo=TRUE, eval=TRUE----------------------------------
data_path <- system.file("extdata", package="NWCCompare")
data_path <- paste(data_path, "modeled", sep="/")
# Note this data path is sample data in the package. 
# You would set the path to the folder containing your streamflow data.
start_year <- "2007"
end_year <- "2010"
localData <- get_local_data(data_path,start_year=start_year,end_year=end_year)
str(localData[1])

## ----getfYourOwn, echo=TRUE, eval=TRUE-----------------------------------
# Here you would load your data. It should have two columns, the first for date
# the second for discharge. A sample is printed below. We also need the EflowStats
# function dataCheck for this example.
library(EflowStats)
Modeled<-mod_data
str(mod_data)
Modeled$date <- as.Date(Modeled$date)
Modeled <- dataCheck(Modeled, yearType = "water")
str(Modeled)

## ----buildfYourOwn, echo=TRUE, eval=TRUE---------------------------------
sites <- names(localData)

da <- read.csv(file = (file.path(data_path, "drainarea.csv")),
               colClasses = c("character","integer"))

drainage_area_sqmi <- da$darea
names(drainage_area_sqmi) <- da$siteNo

peak_threshold <- rep(0, length(sites))
names(peak_threshold) <- sites

for(site in sites) {
  fd <- localData[site][[1]]
}

flow_data_local <- list(daily_streamflow_cfs = localData,
                           drainage_area_sqmi = drainage_area_sqmi)

## ----justStats, echo=TRUE, eval=TRUE-------------------------------------
stats=c("magAverage", "magLow", "magHigh",
        "frequencyLow", "frequencyHigh",
        "durationLow", "durationHigh",
        "timingAverage", "timingLow", "timingHigh",
        "rateChange",
        "magnifSeven", "otherStat")
eflow_stats <- calculate_stats_by_group(stats, flow_data_nwis)
str(eflow_stats, list.len = "10")

## ----justStatsAny, echo=TRUE, eval=FALSE---------------------------------
#  eflow_stats <- calculate_stats_by_group(stats, flow_data_nwc)
#  
#  eflow_stats <- calculate_stats_by_group(stats, flow_data_local)

## ----createstatsdiffs, echo=TRUE, eval=TRUE------------------------------
# Note this could contain many site pairs. 
sites <- data.frame(nwis_sites=nwis, b=huc, stringsAsFactors = FALSE)
diff_stats <- calculate_stats_diffs(sites = sites,
                                       flow_data_a = flow_data_nwis,
                                       flow_data_b = flow_data_nwc,
                                       yearType = "water",
                                       digits = 2)
str(diff_stats, list.len = "10")

## ----viewData, echo=FALSE, eval=TRUE-------------------------------------
diff_stats[,c(1:10)]

## ----saveData, echo=TRUE, eval=FALSE-------------------------------------
#  write.table(diff_stats,
#              file = "diffstats_output.txt",
#              col.names = TRUE,
#              row.names = FALSE,
#              quote = FALSE,
#              sep = "\t")

## ----GoFstats, echo=TRUE, eval=TRUE--------------------------------------
Gaged <- flow_data_nwis$daily_streamflow_cfs["02335757"][[1]]
Modeled <- flow_data_nwc$daily_streamflow_cfs["031300011004"][[1]]
GoF_stats <- calculate_GoF_stats(Gaged, Modeled)
GoF_anmon_stats <- calculate_GoF_summary_stats(Gaged, Modeled)
str(GoF_stats, list.len = "10")

