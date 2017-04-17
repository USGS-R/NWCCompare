#' Function to return NWC Intercomparison portal modeled data for a given site
#'
#' This function accepts a url and returns a data frame of data for that SOS request
#'
#' @param input url for SOS service for desired site data
#' @return data frame containing desired time series with names 'date' and 'data'
#' @importFrom XML xmlTreeParse xpathApply xmlParse xmlValue
#' @importFrom utils read.delim
#' @export
#' @examples
#' parse_swe_csv(system.file('extdata','SWECSVBlock_daymet_example.xml',package='NWCEd'))
parse_swe_csv <- function(input) {
  mod_open <- file(input,open="r")
  content<-paste(readLines(mod_open,warn=FALSE))
  close(mod_open,type="r")
  if (any(grepl('<ExceptionText>invalid parameter</ExceptionText>',content))) {
    stop('An invalid parameter error was encountered. The HUC may not exist.')
  }
  if (length(sapply(content,nchar))>1) {
    dat <- read.delim(header = F, comment.char = "",
                       as.is = T, sep = ",", text = xpathApply(xmlParse(content),
                                                               "//swe:values", xmlValue)[[1]])
    names(dat) <- c('date', 'data')
    dat$date <- as.Date(strptime(dat$date, format = "%Y-%m-%dT%H:%M:%SZ"))
    dat$data <- as.numeric(dat$data)
    dat <- as.data.frame(dat)
    attr(dat, "SRC") <- input
    class(dat) <- c("dat", "data.frame")
    return(dat)
  } else {
    dat<-""
    return(dat)}
}
