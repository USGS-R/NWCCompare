#' Function to return flow data from a locally stored file
#' 
#' This function accepts a list of sites, start and end years 
#' and returns a data frame containing requested flow data
#' 
#' @param data_path path to directory containing data files
#' @param start_year beginning water year, will be translated to 10/01
#' @param end_year ending water year, will be translated to 09/30
#' @param sep_char string containing the datafile separator, default is comma
#' @return dataOut data frame containing requested flow data for the stations
#' @export
#' @importFrom readr read_delim
#' @examples
#' data_path <- system.file("extdata", package="NWCCompare")
#' data_path <- paste(data_path, "modeled", sep="/")
#' start_year <- "2007"
#' end_year <- "2010"
#' localData <- get_local_data(data_path,start_year=start_year,end_year=end_year)
get_local_data <- function(data_path, start_year="", end_year="", sep_char=",") {
  
  yearType <- "water" # could implement both calendar types in the future.
  
  if (nchar(start_year)>1) {start_date <- paste(start_year,"10","01",sep="-")}
  
  if (nchar(end_year)>1) {end_date <- paste(end_year,"09","30",sep="-")}
  
  fileList <- list.files(path=data_path)
  
  for (i in 1:length(fileList)) {
    fileList[i] <- ifelse(nchar(strsplit(fileList[i], ".csv")) < nchar(fileList[i]) | 
                            nchar(strsplit(fileList[i],".txt")) < nchar(fileList[i]), 
                          fileList[i],
                          NA)
  }
  
  fileList <- fileList[which(!is.na(fileList))]
  
  drainFile <- fileList[charmatch("drain",fileList)]
  
  peakFiles <- fileList[charmatch("peak",fileList)]
  
  if (!is.na(peakFiles)) {
    sites <- fileList - peakFiles
    
    sites <- sites[which(!fileList %in% drainFile)]
    
  } else {
    sites <- fileList[which(!fileList %in% drainFile)]
  }
  dataOut <- list()
  
  for (i in 1:length(sites)) {
    x_obs <- read_delim(file = file.path(data_path,sites[i]),
                        delim = sep_char,
                        col_types = "ccd",
                        col_names = TRUE)
    
    site <- unlist(x_obs[1,1])
    x_obs <- as.data.frame(x_obs[,2:3], stringsAsFactors = "false")
    
    if(!dateFormatCheck(unlist(x_obs[,1])[1])){
      x_obs[,1] <- as.character(as.Date(x_obs[,1],format="%m/%d/%Y"))
    }
    
    x_obs[,1] <- as.Date(x_obs[,1])

    if (nchar(start_year)>1) {
      x_obs <- x_obs[which(strptime(x_obs$date,"%Y-%m-%d") >= strptime(start_date,"%Y-%m-%d")),]
    }
    
    if (nchar(end_year)>1) {
      x_obs <- x_obs[which(strptime(x_obs$date, "%Y-%m-%d") <= strptime(end_date, "%Y-%m-%d")),]
    }
    
    obs_data <- dataCheck(x_obs, yearType = "water")

    dataOut[[site]]<-obs_data
  }
  return(dataOut)
}

dateFormatCheck <- function(date_ex){  # checks for the format YYYY-MM-DD
  parts <- strsplit(date_ex,"-",fixed=TRUE)
  condition <- FALSE
  if (length(parts[[1]])>1) {
    if (nchar(parts[[1]][1]) == 4 && nchar(parts[[1]][2]) == 2 && nchar(parts[[1]][3]) == 2){
      testYear <- as.numeric(parts[[1]][1])
      testMonth <- as.numeric(parts[[1]][2])
      testDay <- as.numeric(parts[[1]][3])
      if (!is.na(testYear) && !is.na(testMonth) && !is.na(testDay)){
        if (testMonth <= 12 && testDay <= 31){
          condition <- TRUE
        }        
      }      
    }
  }
  return(condition)
}