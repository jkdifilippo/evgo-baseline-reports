################################## HEADER ######################################
# purpose: process vehicle registration data
# project: EVgo Evaluation Project
#    year: 2019
#
#     org: UCLA Luskin Center for Innovation
# website: innovation.luskin.ucla.edu
#
#  author: James Di Filippo
#   email: jdifilippo@luskin.ucla.edu

########################## DEPENDENCIES/OPTIONS/KEYs ###########################

## load dependencies
source("src/dependencies.R")

############################### SCRIPT #########################################

## disaggregated vehicle registration data is a proprietary data product sold by
## IHS Markit and thus cannot be shared. Aggregated vehicle data that is the
## result of this script is included in the repo. If the processed file is
## present, that file is loaded instead


# proccessed data not foudnd----------------------------------------------------
if (!file.exists("data/processed/bev-data.csv")) {
 
# data/inputs ------------------------------------------------------------------
  
    load("data/processed/image-files/service-areas.RData")
  
    vehicles_raw <- read_csv("data/raw/vehicle-data/pev-reg-statewide.csv")
    attributes <- read_csv("data/raw/vehicle-data/vehicle-properties.csv")

# process bev data -------------------------------------------------------------
    
    ## join vehicle data with attribute file to identify and filter for DCFC
    ## capable bevs
    dcfc_capable_bevs <- 
        left_join(vehicles_raw, attributes) %>% 
        filter(max_dc_charging > 0)
        
    ## identify whether bev is Tesla or non-Tesla BEV and summarize by census
    ## tract geoid
    bev_summary <- dcfc_capable_bevs %>% 
        rename(geoid = census_tract) %>% 
        mutate(tesla = ifelse(make == "TESLA", "Tesla", "Other\nBEV")) %>% 
        group_by(geoid, tesla) %>% 
        summarize(bev_num = sum(total)) %>% 
        ungroup()
    
    ## join bev census tract summary to local service area census tract geoids
    ## and then summarize by each property/isochrone
    bevs <- 
      left_join(bev_summary, local_service_area) %>% 
      filter(is.na(property) == FALSE) %>% 
      mutate(bev_num = (bev_num * allocation) %>% round(digits = 0)) %>% 
      group_by(property, contour, tesla) %>% 
      summarize(bev_num = sum(bev_num)) %>% 
      ungroup()

# processed data found ---------------------------------------------------------
} else { 

# load processed data ----------------------------------------------------------
    bevs <- read_csv("data/processed/bev-data.csv")
}

############################## OUTPUTS #########################################

write_csv(bevs,"data/processed/bev-data.csv")

rm(list=ls()[! ls() %in% c("bevs")])
save.image("data/processed/image-files/vehicles.RData")

############################## CLEANUP #########################################
rm(list=ls())

