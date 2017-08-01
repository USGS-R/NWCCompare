#' Sample observed data for demonstration purposes
#'
#' @name obs_data
#' @docType data
#' @keywords data
NULL
#'Sample modeled data for demonstration purposes
#'
#' @name mod_data
#' @docType data
#' @keywords data
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and
is subject to revision. It is being provided to meet
the need for timely best science. The information
has not received final approval by the U.S. Geological
Survey (USGS) and is provided on the condition that
neither the USGS nor the U.S. Government shall be held
liable for any damages resulting from the authorized
or unauthorized use of the information.

****Support Package****
This package is a USGS-R Support package. 
see: https://owi.usgs.gov/R/packages.html#support")
}