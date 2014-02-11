runStatsGroups<-function(x_data,tempArrays,i,drain_area){
  if (nrow(x_data) > 2) {
    flow_data <- get_obsdata(x_data)
    tempArrays$min_date[i] <- as.character(min(flow_data$date))
    tempArrays$max_date[i] <- as.character(max(flow_data$date))
    flow_data <- flow_data[, c("wy_val", "date", "discharge", "month_val", "year_val", "day_val", "jul_val")]
    if (ncol(tempArrays$ObsFlowStats) > 0) {
      tempArrays$ObsFlowStats[i, ] <- FlowStatsAll(flow_data, drain_area, stats=stats)
    }
    if (ncol(tempArrays$magnifSevenObs) > 0) {
      tempArrays$magnifSevenObs[i, ] <- magnifSeven(flow_data)
    }
    tempArrays$comment <- ""
  } else {
    tempArrays$comment[i] <- "No observed data for this site"
  }
  return(tempArrays)
}