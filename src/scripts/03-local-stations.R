################################## HEADER ######################################

# purpose: define and develop baseline service areas
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

## census api key
census_api_key(Sys.getenv("census_apikey"))

## set options
options(stringsAsFactors = FALSE)

################################## data ########################################

load("service_areas.RData")

# get afdc station data from data.gov API -------------------------------------------

## api access is free but requires api key which can be obtained here:
## https://api.data.gov/signup/
afdc <- 
  GET(url = "https://developer.nrel.gov/api/alt-fuel-stations/v1.json",
      query = list(fuel_type = "ELEC",
                   state = "CA",
                   format = "JSON",
                   api_key = Sys.getenv("afdc_apikey"))
      ) %>%
  content("text") %>% 
  fromJSON() %>%
  pluck("fuel_stations") %>% 
  as_tibble() 

################################ SCRIPT ########################################

## remove unuseful variables and filter for public stations
pub_afd <- afdc %>% 
    select(id,
         station_name,
         access_code, 
         latitude, 
         longitude, 
         ev_connector_types, 
         ev_dc_fast_num, 
         ev_level1_evse_num, 
         ev_level2_evse_num, 
         ev_network) %>% 
    filter(access_code == "public") %>% 
  
    ## remove HCPC in afdc
    filter(!str_detect(station_name, "Valley Vill|Chevron Will|Southside P"))

# DCFC charging stations -------------------------------------------------------

## Determines how many DCFC statsions are within 1 mile of the
## boundary of each service area by drawing a 1 mile buffer around charging
## station locations and testing whether those buffers intersect with the
## service area polygons.


## Filter for DCFC, categorize dcfc into tesla/evgo/other and create simple
## features object from coordinates
dcfc_sf <- pub_afdc_sub %>% 
    filter(!is.na(ev_dc_fast_num)) %>%
    mutate(
      charge_network = case_when(str_detect(ev_network, "Tesla") ~ "Tesla",
                                    ev_network == "eVgo Network" ~ "EVgo",
                                                            TRUE ~ "Other")
    ) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(32610)

## Create one-mile buffer around dcfc stations
dcfc_buffer <- dcfc_sf %>% 
  st_buffer(1609.34) ## 1 mile ~ 1609.34 meters

# join DCFC buffers to service area shapes to identify overlap (charger in
# service area)
dcfc_service_area <- 
  st_join(x = dcfc_buffer,
          y = local_service_area,
          join = st_intersects) %>% 
  filter(is.na(property) == FALSE) %>%
  st_drop_geometry() %>% 
  distinct(id,property,contour, .keep_all = TRUE)

## create summary stats for each service area time interval and charge network
dcfc_summary <- dcfc_service_area %>% 
  group_by(property, contour, charge_network) %>% 
  summarise(dcfc = sum(ev_dc_fast_num)) %>% 
  ungroup()

# L2 charging stations ---------------------------------------------------------

## Determines how many L2 statsions are within 1/2 mile of the boundary of each
## service area by drawing a 1/2 mile buffer around charging station locations
## and testing whether those buffers intersect with the service area polygons.

## filter for level 2 chargers and create simple features object from their
## coordignates
l2_sf <- 
  pub_afdc_sub %>% 
  filter(is.na(ev_level2_evse_num) == FALSE) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(32610)


## create half-mile buffer around l2 stations
l2_buffer <- l2_sf %>% 
  st_buffer(1609.34 * 0.5) ## 1/2 mile `` 1/2 * 1609.34 meters

# join l2 buffers to service area shapes to identify overlap (charger in service
# area)
l2_service_area <- 
  st_join(x = l2_buffer,
          y = local_service_area,
          join = st_intersects) %>% 
  filter(!is.na(property)) %>%
  st_drop_geometry() %>% 
  distinct(id, property, contour, .keep_all = TRUE)

## sum the number of l2 chargers in each service area
l2_summary <- 
  l2_service_area %>% 
  group_by(property, contour) %>% 
  summarise(l2_chargers = sum(ev_level2_evse_num)) %>% 
  ungroup()

################################# OUTPUT #######################################

## save image file with relevant objects
rm(list=ls()[! ls() %in% c("l2_summary", "dcfc_summary", "dcfc_sf", "l2_sf")])
save.image(file = "stations.RData")

################################# CLEANUP ######################################

rm(list=ls())