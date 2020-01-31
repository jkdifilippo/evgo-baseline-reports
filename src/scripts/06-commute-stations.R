library(raster)
library(sf)
library(tidyverse)
library(mapview)
library(scales)

load("stations.RData")
load("muds.RData")
load("service_areas.RData")

l2_join <- 
  st_join(mud_commuters, l2_buffer, join = st_intersects) %>% 
  filter(mud_commuters != 0) %>% 
  mutate(ev_level2_evse_num = ifelse(is.na(ev_level2_evse_num) == TRUE, 0, ev_level2_evse_num)) %>% 
  group_by(property.x, mud_commuters, geoid) %>% 
  summarize(l2_chargers = sum(ev_level2_evse_num)) %>% 
  group_by(property.x, l2_chargers) %>% 
  summarize(mud_commuters = sum(mud_commuters) %>% round(digits = 0)) %>% 
  ungroup()

dcfc_join_freq <- 
  st_join(mud_commuters, dcfc_buffer, join = st_intersects) %>% 
  st_drop_geometry() %>%
  dplyr::select(geoid, charge_network, property = property.x, mud_commuters, ev_dc_fast_num)

run_properties <- function(property_list) {

dcfc <- dcfc_join_freq[dcfc_join_freq$property == property_list,]

map_df(unique(dcfc$geoid), expand_zeros, data = dcfc)
}

expand_zeros <- function(tractid, data) {
ex <- data[data$geoid == tractid,]

if (!any(ex$charge_network == "Tesla", na.rm = TRUE)) {
  ex <- ex[1,] %>% 
        mutate(charge_network = "Tesla", ev_dc_fast_num = 0) %>%
        bind_rows(ex)
}

if (!any(ex$charge_network == "Other", na.rm = TRUE)) {
    ex <- ex[1,] %>% 
    mutate(charge_network = "Other", ev_dc_fast_num = 0) %>%
    bind_rows(ex)
}

if (!any(ex$charge_network == "EVgo", na.rm = TRUE)) {
  ex <- ex[1,] %>% 
    mutate(charge_network = "EVgo", ev_dc_fast_num = 0) %>%
    bind_rows(ex)
}

return(ex)

}

dcfc_stations_commute <- map_df(unique(dcfc_join_freq$property), run_properties) %>% 
  filter(!is.na(charge_network)) %>% 
  rename(chargers = ev_dc_fast_num) %>% 
  group_by(property, geoid, charge_network) %>% 
  summarize(commuters = mean(mud_commuters), chargers = sum(chargers))

rm(list=ls()[! ls() %in% c("dcfc_join", "l2_join", "dcfc_join_freq", "dcfc_stations_commute")])
save.image(file = "commute_chargers.RData")
