#' Function to calculate the differences in  statistics for given observed and modeled data sets
#' 
#' This function accepts observed and modeled data frames of daily flow data and returns a data frame of 
#' calculated diff statistics
#' 
#' @param sites list of usgs station ids
#' @param startdate startdate for pulling data
#' @param enddate enddate for pulling data
#' @param X_DATA_FUN function for pulling data from x_args
#' @param x_args urls for pulling observed data
#' @param DRAIN_AREA_FUN function for pulling drainage area
#' @param drain_args url for pulling drainage area
#' @param M_DATA_FUN function for pulling modeled data form m_args
#' @param m_args url for pulling modeled data
#' @return statsout data frame of calculated statistics
#' @import XML
#' @import zoo
#' @import chron
#' @import doBy
#' @import lmomco
#' @importFrom stats aggregate
#' @export
calculateStatsDiffs<-function(sites, startdate, enddate, X_DATA_FUN, x_args, DRAIN_AREA_FUN, drain_args, M_DATA_FUN, m_args) {
  supportedStats=getSupportedStatNames()
  stats="GoF"
  tempArrays<-getEmptyResultArrayNWCStats(stats, length(sites), supportedStats)
  for (i in 1:length(sites)) {
    site = sites[i]
    m_data <- M_DATA_FUN(m_args[i])
    if (nrow(m_data)>2) {
      startdate<-min(m_data$date)
      enddate<-max(m_data$date)
      interval<-''
      latest<-''
      x_data <- X_DATA_FUN(x_args[i])   
      if (nrow(x_data)>2) {
        obs_data <- get_obsdata(x_data)
        obs_count<-nrow(x_data)
        cat(paste("get_obsdata run on x_obs for site",site,obs_count,"\n",sep=" "))
        m_data$date <- as.Date(m_data$date,format="%Y-%m-%d")
        m_data<-m_data[m_data$date>=min(x_data$date) & m_data$date<=max(x_data$date), ]
      drain_area<-DRAIN_AREA_FUN(drain_args[i])
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
    t<- SiteGoF(obs_data, mod_data)
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