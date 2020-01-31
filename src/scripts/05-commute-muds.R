library(tidyverse)
library(sf)
library(tidycensus)
library(scales)
library(forcats)
library(magrittr)
library(ggspatial)
library(ggmap)

options(stringsAsFactors = FALSE)

load("service_areas.RData")

commuter_service_area <- 
  commuter_service_area %>% 
  rename(geoid = h_geoid)
  
census_api_key(Sys.getenv("census_apikey"))

# census varlist---------------------------
census_vars <- 
  read_csv("census_pull.csv") %>% 
  mutate(var_type = tolower(var_type),
         var_name = var_name %>% 
           str_remove("(Estimate\\!\\!Total\\!\\!)(?=.)") %>% 
           str_remove("^(Estimate\\!\\!)") %>% 
           str_replace_all("\\!?\\!", " ") %>% 
           str_replace_all("(?<=[0-9])\\s(?=[0-9])", ",") %>% 
           str_replace("(or more)", "+")
  )

# functions ------------------------------------

acs_commuter_area <- function (variablenames,
                            vars = census_vars,
                            service_area = commuter_service_area) {
  
    c_data <- 
    get_acs(geography = "tract",
            variables = vars %>%
              filter(var_type %in% variablenames) %>% 
              pull(variable), 
            state = "CA") %>% 
    left_join(vars) %>% 
    rename_all(tolower) %>% 
    select(geoid, var_name, estimate)
  
return(c_data)
  
}

commuter_mud <- 
  acs_commuter_area("units in structure") %>% 
  filter(var_name != "Total") %>% 
  mutate(residence = case_when(
                       str_detect(var_name, "[3-9]") == TRUE ~ "mud",
                       TRUE ~ "non_mud")) %>% 
  group_by(geoid, residence) %>% 
  summarise(estimate = sum(estimate)) %>%
  group_by(geoid) %>% 
  mutate(share = estimate/sum(estimate)) %>%
  filter(residence == "mud") %>% 
  select(geoid, share) %>% 
  ungroup

mud_commuters <- 
  commuter_service_area %>% 
  left_join(commuter_mud) %>% 
  mutate(share = if_else(is.nan(share), 0, share),
         mud_commuters = share * commuters)

mud_com_xy <- 
  mud_commuters%>% 
  st_centroid() %>% 
  st_transform(4326) %>% 
  st_coordinates() %>% 
  as_tibble()

mud_com_map <- 
  mud_commuters %>%
  as_tibble() %>% 
  select(-geometry) %>% 
  rename(property = property.x) %>%
  bind_cols(mud_com_xy) %>% 
  left_join(station_list_geocode)


rm(list=ls()[! ls() %in% c("mud_com_map", "mud_commuters")])
save.image(file = "muds.RData")

