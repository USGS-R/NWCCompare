
install.packages(c("zoo","chron","doBy","XML","hydroGOF","lmomco","RCurl"))
install.packages(c("EflowStats","NWCCompare"),repos="http://usgs-r.github.com",type="source")

library(EflowStats)
library(NWCCompare)

# Run stats and differences on USGS observed and modeled daily discharge data
# Cooresponds to this page: https://cida.usgs.gov/nwc/#!waterbudget/achuc/031601020108
# See test_calculateStatsDiffs

# Run stats on modeled huc12s
# See test_calculateStatsGroups

# Run stats on USGS observed daily discharge data
# See test_calculateStatsGroups