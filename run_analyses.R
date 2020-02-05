################################# HEADER #######################################

#   script: Automation script for EVgo Baseline Stats Reports
#  project: EVgo Evaluation Project - Baseline Reports
#     year: 2019
#
#   author: James Di Filippo
#    email: jdifilippo@luskin.ucla.edu
#
#      org: UCLA Luskin Center for Innovation
#  website: innovation.luskin.ucla.edu

############################## DEPENDENCIES ####################################
source("src/dependencies.r")

########################### PROJECT STRUCTURE ##################################

## builds file structure not contained in git repository
folder_names <- 
  c("data/processed",
    "data/raw/shapefiles",
    "data/raw/vehicle-data",
    "output",
    "output/datasets",
    "reports")

map(folder_names, dir.create, showWarnings = FALSE)

################################ SCRIPTS #######################################




########################### GENERATE REPORTS ###################################




############################### CLEANUP ########################################