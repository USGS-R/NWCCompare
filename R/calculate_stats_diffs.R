#' Function to calculate the differences in  statistics for given observed and modeled data sets
#' 
#' This function accepts observed and modeled data frames of daily flow data and returns a data frame of 
#' calculated diff statistics
#' 
#' @param sites A two column dataframe containing site names for flow_data_a
#' and flow_data_b flow data.
#' @param flow_data_a A dataframe containing a NWCCompare flow dataset. 
#' Should have been cleaned by \link[EflowStats]{dataCheck}
#' @param flow_data_b A second NWCCompare flow dataset to be compared
#' to flow_data_a. 
#' @return statsout data frame of calculated statistics
#' @importFrom stats aggregate
#' @export
#' @examples
#' # https://cida.usgs.gov/nwc/#!waterbudget/achuc/031300011004
#' nwis <- "02335757"
#' huc <- "031300011004"
#' sites <- data.frame(a=nwis, b=huc)
#' flow_data_a <- build_nwis_dv_dataset(nwis, start_date = "2004-10-01", end_date = "2010-09-30")
#' flow_data_b <- build_nwc_flow_dataset(huc, start_date = "2004-10-01", end_date = "2010-09-30")
#' 
#' 
calculate_stats_diffs<-function(sites, flow_data_a, flow_data_b) {
  for (i in 1:length(sites)) {
    site = sites[i]
    m_data <- M_DATA_FUN(m_args[i])
    m_data <- m_data$discharge
    names(m_data) <- c("date", "discharge")
    if (nrow(m_data)>2) {
      startdate<-min(m_data$date)
      enddate<-max(m_data$date)
      interval<-''
      latest<-''
      x_data <- X_DATA_FUN(siteNumber = x_args[i], 
                           parameterCd = "00060",
                           startDate = startdate,
                           endDate = enddate)   
      if (nrow(x_data)>2) {
        obs_data <- get_obsdata(x_data)
        obs_count<-nrow(obs_data)
        cat(paste("get_obsdata run on x_obs for site",site,obs_count,"\n",sep=" "))
        m_data$date <- as.Date(m_data$date,format="%Y-%m-%d")
        m_data<-m_data[m_data$date>=min(obs_data$date) & m_data$date<=max(obs_data$date), ]
        drain_area<-DRAIN_AREA_FUN(drain_args[i])
        drain_area <- as.numeric(drain_area$drain_area_va)
        cat(paste("data and drainage area retrieved for site",site,drain_area,"\n",sep=" "))
        mod_data <- get_obsdata(m_data)
        mod_count <- nrow(mod_data)
        cat(paste("get_obsdata run on x_mod for site",site,mod_count,"\n",sep=" "))
        countbyyr<-aggregate(obs_data$discharge, list(obs_data$wy_val), length)
        countbyyr_mod<-aggregate(mod_data$discharge, list(mod_data$wy_val), length)
        colnames(countbyyr)<-c('wy','num_samples')
        colnames(countbyyr_mod)<-c('wy','num_samples')
        sub_countbyyr<-subset(countbyyr,countbyyr$num_samples >= 365)
        sub_countbyyr_mod<-subset(countbyyr_mod,countbyyr$num_samples >= 365)
        include_yrs<-merge(sub_countbyyr,sub_countbyyr_mod)
        if (nrow(include_yrs)==0) {
          tempArrays$comment[i]<-"No matching complete water years for site"
        } else {
          obs_data<-merge(obs_data,include_yrs,by.x="wy_val",by.y="wy")
          mod_data<-merge(mod_data,include_yrs,by.x="wy_val",by.y="wy")
          obs_data<-obs_data[order(obs_data$jul_val),]
          mod_data<-mod_data[order(mod_data$jul_val),]
          obs_count <- nrow(obs_data)
          mod_count <- nrow(mod_data)
          if (length(mod_data$discharge)<3) { 
            tempArrays$comment[i]<-"No matching complete water years for site" 
          } else { 
            if (length(mod_data$discharge)!=length(obs_data$discharge)) { 
              tempArrays$comment[i]<-"Observed and modeled time-series don't match for site"
            } else {
              cat(paste("data sets merged for site",site,obs_count,mod_count,"\n",sep=" "))
              tempArrays$min_date[i] <- as.character(min(obs_data$date))
              tempArrays$max_date[i] <- as.character(max(obs_data$date))
              tempArrays$comment[i] <- ""
              cat(paste("dates calculated for site",site,"\n",sep=" "))
              
              obs_data <- obs_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
              mod_data <- mod_data[,c('wy_val','date','discharge','month_val','year_val','day_val','jul_val')]
              obs_count <- nrow(obs_data)
              mod_count <- nrow(mod_data)
              cat(paste("dfs created for site",site,obs_count,mod_count,"\n",sep=" "))
              tempArrays$ObsStats[i, ] <- FlowStatsICP(obs_data,drain_area)
              tempArrays$ModStats[i, ] <- FlowStatsICP(mod_data,drain_area)
              tempArrays$DiffStats[i, ] <- (tempArrays$ModStats[i, ]-tempArrays$ObsStats[i, ])/tempArrays$ObsStats[i, ]
              cat("diffs calculated \n")
              t<- calculate_GoF_stats(obs_data, mod_data)
              tempArrays$GoFStats[i, ] <- t[,2:ncol(t)]
            }
          }}
      } else {
        tempArrays$comment[i]<-"No observed data for this site"
      }
    } else { 
      tempArrays$comment[i]<-"No modeled data for site"
    } 
  }
  statsout<-nameStatsArray("GoF", sites, tempArrays)
  return(statsout)
}