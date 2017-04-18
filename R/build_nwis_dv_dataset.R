#' Builds a date, streamflow, drainage area dataset of NWIS site data.
#'
#' Takes a list of sites and a start and end date of interest. Returns
#' a named list containing dates, streamflow, and drainage area. Units are 
#' square miles and cubic feet per second.
#'
#' @param sites A character vector of NWIS sites.
#' @return start_date A string representation of the start date in YYYY-MM-DD format.
#' @return end_date A string representation of the end date in YYYY-MM-DD format.
#' @importFrom dataRetrieval readNWISdv readNWISsite readNWISpeak
#' @importFrom EflowStats dataCheck peakThreshold
#' @export
#' @examples
#' sites <- c("02177000","02178400")
#' start_date <- "2008-10-01"
#' end_date <- "2010-09-30"
#' build_nwis_dv_dataset(sites, start_date, end_date)

build_nwis_dv_dataset <- function(sites, start_date, end_date) {
  
  nwis_dataset <- rep(list(list()), length(sites))
  names(nwis_dataset) <- sites
  
  drainage_area_sqmi <- rep(NA, length(sites))
  names(drainage_area_sqmi) <- sites
  
  peak_threshold <- rep(NA, length(sites))
  names(peak_threshold) <- sites
  
  for(site in sites) {
    streamflow <- readNWISdv(siteNumber = site, 
                            parameterCd = "00060",
                            startDate = start_date,
                            endDate = end_date)
    
    nwis_dataset[site] <- list(dataCheck(streamflow[c("Date","X_00060_00003")],yearType="water"))
    
    drainage_area_sqmi[site] <- as.numeric(readNWISsite(site)$drain_area_va)
  
    peakFlows <- readNWISpeak(siteNumber = site,
                              startDate = start_date,
                              endDate = end_date)
    
    peak_threshold[site] <- peakThreshold(nwis_dataset[site][[1]][c("date","discharge")],
                                          peakFlows[c("peak_dt","peak_va")])
  }
  
  output <- list(daily_streamflow_cfs = nwis_dataset, 
                 drainage_area_sqmi = drainage_area_sqmi,
                 peak_threshold_cfs = peak_threshold)
}
