library(tidyverse)
library(sf)

# local service areas --------------------------------
load("service_areas.RData")


#vehicles
vehicles <- 
  read_csv("vehicles_conf/pev_reg_statewide.csv")

attrib <- 
  read_csv("vehicles_conf/vehicle_properties.csv")

dc_fast_cap <- 
  left_join(vehicles, attrib) %>% 
  filter(max_dc_charging > 0) %>%
  rename(geoid = census_tract) %>% 
  mutate(tesla = ifelse(make == "TESLA", "Tesla", "Other\nBEV")) %>% 
  group_by(geoid, tesla) %>% 
  summarize(bev_num = sum(total)) %>% 
  ungroup()

dc_fast_bev <- 
  left_join(dc_fast_cap, local_service_area) %>% 
  filter(is.na(property) == FALSE) %>% 
  mutate(bev_num = (bev_num * allocation) %>% round(digits = 0)) %>% 
  group_by(property, contour, tesla) %>% 
  summarize(bev_num = sum(bev_num)) %>% 
  ungroup()

write_csv(dc_fast_bev, "local_data/dc_fast_bev.csv")

rm(list=ls()[! ls() %in% c("dc_fast_bev")])
save.image("vehicles.RData")

