library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(mapview)

# local service areas --------------------------------
load("service_areas.RData")

# get afdc station data -------------------

afdc <- 
  GET(url = "https://developer.nrel.gov/api/alt-fuel-stations/v1.json",
      query = list(fuel_type = "ELEC",
                   state = "CA",
                   format = "JSON",
                   api_key = Sys.getenv("afdc_apikey"))
              # api key signup: https://api.data.gov/signup/
      ) %>%
  content("text") %>% 
  fromJSON() %>%
  pluck("fuel_stations") %>% 
  as_tibble() 

pub_afdc_sub <- afdc %>% 
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
  
  #remove HCPC in afdc
  filter(str_detect(station_name, "Valley Village|Chevron Willow|Southside Park") != TRUE)

dcfc_sf <- 
  pub_afdc_sub %>% 
  filter(!is.na(ev_dc_fast_num)) %>%
  mutate(charge_network = case_when(
                      str_detect(ev_network, "Tesla") == TRUE ~ "Tesla",
                      ev_network == "eVgo Network" ~ "EVgo",
                      TRUE ~ "Other")
        )%>% 

  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(32610)

mapview(dcfc_sf)


l2_sf <- 
  pub_afdc_sub %>% 
  filter(is.na(ev_level2_evse_num) == FALSE) %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(32610)

dcfc_buffer <- 
  dcfc_sf %>% 
  st_buffer(1609.34) ## 1 mile

l2_buffer <- 
  l2_sf %>% 
  st_buffer(1609.34 * 0.5) ## 1/2 mile

l2_service_area <- 
  st_join(x = l2_buffer,
          y = local_service_area,
          join = st_intersects) %>% 
  filter(is.na(property) == FALSE) %>%
  as_tibble() %>% 
  distinct(id,property,contour, .keep_all = TRUE)

l2_summary <- 
  l2_service_area %>% 
  group_by(property, contour) %>% 
  summarise(l2_chargers = sum(ev_level2_evse_num)) %>% 
  ungroup()

dcfc_service_area <- 
  st_join(x = dcfc_buffer,
          y = local_service_area,
          join = st_intersects) %>% 
  filter(is.na(property) == FALSE) %>%
  as_tibble() %>% 
  distinct(id,property,contour, .keep_all = TRUE)

dcfc_summary <- 
  dcfc_service_area %>% 
  group_by(property, contour, charge_network) %>% 
  summarise(dcfc = sum(ev_dc_fast_num)) %>% 
  ungroup()

write_csv(l2_summary, "local_data/l2_summary.csv")
write_csv(dcfc_summary, "local_data/dcfc_summary.csv")


rm(list=ls()[! ls() %in% c("l2_buffer","dcfc_buffer", "l2_summary", "dcfc_summary", "dcfc_sf", "l2_sf")])
save.image(file = "stations.RData")
