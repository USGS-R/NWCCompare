#' Builds a date, streamflow, drainage area dataset of NWC modeled data.
#'
#' Takes a list of hucs and a start and end date of interest. Returns
#' a named list containing dates, streamflow, and drainage area. Units are 
#' square miles and cubic feet per second.
#'
#' @param hucs A character vector of 12-digit HUCs.
#' @param start_date A string representation of the start date in YYYY-MM-DD format.
#' @param end_date A string representation of the end date in YYYY-MM-DD format.
#' @importFrom EflowStats dataCheck peakThreshold
#' @export
#' @examples
#' hucs <- c("031601020108","031501100104")
#' start_date <- "2008-10-01"
#' end_date <- "2010-09-30"
#' build_nwc_flow_dataset(hucs, start_date, end_date)
build_nwc_flow_dataset <- function(hucs, start_date="1980-10-01", end_date="2010-09-30") {
  
  if(any(nchar(hucs)!=12)) stop("Must submit 12-digit HUC ids")
  
  nwc_dataset <- rep(list(list()), length(hucs))
  names(nwc_dataset) <- hucs
  
  drainage_area_sqmi <- rep(NA, length(hucs))
  names(drainage_area_sqmi) <- hucs
  
  peak_threshold <- rep(NA, length(hucs))
  names(peak_threshold) <- hucs
  
  for(huc in hucs) {
    streamflow <- get_nwc_wb_data(huc, start_date = start_date, end_date = end_date,
                                  local = FALSE, return_var = "discharge")
    
    fData <- dataCheck(streamflow$discharge[c("date", "data")],yearType="water")
    nwc_dataset[huc] <- list(fData)
    
    drainage_area_sqmi[huc] <- as.numeric(get_nwc_huc(huc)$features[[1]]$properties$areasqkm) * 0.386102 # convert to sqmi
    
    year_vals <-unique(fData$year_val)
    peak_flows <- data.frame(peak_date = rep(as.Date("1970-01-01"), length(year_vals)),
                             peak_cfs = rep(NA, length(year_vals)))
    
    for(i in 1:length(year_vals)) {
      year_data <- fData[c("date", "discharge")][which(fData$year_val == year_vals[i]),]
      peak_inds <- which(year_data$discharge == max(year_data$discharge))
      if(length(peak_inds) > 1) warning(paste("Flow peaks found on more than one dates:\n", 
                                              paste(fData$date[peak_inds], collapse = ", "), 
                                              "\n The first peak will be used."))
      peak_flows$peak_cfs[i] <- year_data$discharge[peak_inds[1]]
      peak_flows$peak_date[i] <- year_data$date[peak_inds[1]]
    }

    peak_threshold[huc] <- peakThreshold(fData[c("date","discharge")],
                                          peak_flows[c("peak_date","peak_cfs")])
  }
  
  output <- list(daily_streamflow_cfs = nwc_dataset, 
                 drainage_area_sqmi = drainage_area_sqmi,
                 peak_threshold_cfs = peak_threshold)
}
