library(raster)
library(tidyverse)
library(sf)
library(scales)
library(ggspatial)
library(viridis)
library(knitr)
library(isotone)

options(scipen = 999)

## load datasets
load("local_data/census.RData")
load("service_areas.RData")
load("stations.RData")
load("vehicles.RData")
load("muds.RData")
load("commute_chargers.RData")



base_summary <- 
  bind_rows(
    final_station_list %>% mutate(contour = 5),
    final_station_list %>% mutate(contour = 7),
    final_station_list %>% mutate(contour = 10)
  )


summary_dataset <- 
  base_summary %>% 
  left_join(dc_fast_bev %>% 
              pivot_wider(id_cols = c("property", "contour"),
                          names_from = "tesla",
                          names_prefix = "bev_",
                          values_from = "bev_num")
  ) %>% 
 
  left_join(dcfc_summary %>% 
              pivot_wider(id_cols = c("property", "contour"),
                          names_from = "charge_network",
                          names_prefix = "dcfc_num_",
                          values_from = "dcfc")
  ) %>% 
   
  left_join(l2_summary) %>% 
  
  left_join(units_buildings %>% 
              group_by(property, contour) %>%
              summarise(mud_units = sum(estimate)) %>% 
              ungroup()
  ) %>%
  
  left_join(renter_share %>% 
              pivot_wider(id_cols = c("property", "contour"),
                          names_from = "var_name",
                          values_from = "percentage")
  ) %>%
  
  left_join(population %>%
            select(property,
                   contour,
                   population = estimate)) %>% 
  
  left_join(race_ethnicity %>% 
              pivot_wider(id_cols = c("property", "contour"),
                          names_from = "re_group",
                          names_prefix = "percent_",
                          values_from = "percent")
  ) %>%
  
  left_join(hh_income %>% 
              pivot_wider(id_cols = c("property", "contour"),
                          names_from = "income_group",
                          names_prefix = "percent_",
                          values_from = "percent")
  ) %>%
  
  left_join(education %>% 
              pivot_wider(id_cols = c("property", "contour"),
                          names_from = "education_group",
                          names_prefix = "percent_",
                          values_from = "percent")
  ) %>% 

  left_join(mud_commuters %>% 
              st_drop_geometry() %>% 
              select(property = property.x,
                     mud_commuters) %>% 
              group_by(property) %>% 
              filter(!is.nan(mud_commuters)) %>% 
              summarise(mud_commuters = sum(mud_commuters)) %>% 
              ungroup()
  ) %>% 

  left_join(
    dcfc_stations_commute %>% 
              group_by(property, geoid) %>% 
              summarise(chargers = sum(chargers), commuters = mean(commuters)) %>% 
              ungroup() %>% 
              filter(!is.nan(commuters)) %>% 
              group_by(property) %>% 
              summarise(median_commute_dcfc = weighted.median(chargers, commuters))
            ) %>% 
  
  rename_all(tolower) %>% 
  rename_all(~str_replace_all(.,"\\s|/", "_")) %>%
  rename_all(~str_replace_all(., "__", "_")) %>% 
  rename_all(~str_remove_all(.,"\\$|,|-|/n")) %>% 
  filter(!is.na(mud_units)) %>% 
  rename(travel_time = contour)

write_csv(summary_dataset, "summary_dataset.csv")

rm(list=ls()[! ls() %in% c("summary_dataset")])
save.image(file = "summary_data.RData")
  
