# package dependencies for evgo-baseline-reports
# EVgo Evaluation Project
# 2019

# UCLA Luskin Center for Innovation
# innovation.luskin.ucla.edu

# James Di Filippo
# jdifilippo@luskin.ucla.edu

# libraries -------------------------------------------------------------------

## this project requires the following libraries which must be installed prior
## to use.

# geospatial packages ---------------------------------------------------------
require(raster)
require(sf)
require(ggmap)
require(geojsonsf)
library(ggspatial)

# tidyverse -------------------------------------------------------------------
require(tidyverse)
require(readxl)
require(httr)

# api wrappers ----------------------------------------------------------------
require(tidycensus)

#plotting ---------------------------------------------------------------------
require(scales)
require(viridis)

# misc ------------------------------------------------------------------------
require(R.utils)
require(jsonlite)
require(isotone)

# markdown --------------------------------------------------------------------
require(knitr)
