
library(raster)
library(tidyverse)
library(sf)
library(tidycensus)
library(scales)
library(forcats)
library(magrittr)

options(stringsAsFactors = FALSE)

rm(list=ls())

# functions -----------------------------------------------
acs_local_area <- function (variablenames,
                            vars = census_vars,
                            service_area = local_service_area) {
  
  c_data <- 
    get_acs(geography = "tract",
            variables = vars %>%
            filter(var_type %in% variablenames) %>% 
            pull(variable), 
            state = "CA") %>% 
  left_join(vars) %>% 
  rename_all(tolower) %>% 
  select(geoid, var_name, estimate)

  sa_data <- 
    left_join(service_area, c_data) %>% 
      as_tibble() %>% 
      mutate(est_alloc = (allocation * estimate) %>% round(digits = 0)) %>% 
      group_by(property, contour, var_name) %>% 
      summarise(estimate = sum(est_alloc)) %>% 
      ungroup()
  
  return(sa_data)

}

# load local areas ----------------------------
local_service_area <- 
  st_read("service_areas/local_service_area.geojson")

# vehicles

# Census data

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

# MUD unit summary --------------------------------------------------------
units_buildings <- 
  acs_local_area("units in structure") %>% 
    filter(str_detect(var_name, "[3-9]") == TRUE) %T>%
    write_csv("local_data/units_buildings.csv")
  
  
# income summary ----------------------------------------------------------
hh_income <- 
  acs_local_area("household income") %>%
  mutate(upperbin = var_name %>%
                    str_remove_all("( \\+)|\\$|,") %>%
                    str_extract("\\d{5,6}$") %>% 
                    as.numeric(),

        income_group = 
          case_when(upperbin < 75000 ~ "less than\n$75,000",
                                 upperbin < 150000 ~ "$75,000 -\n$150,000",
                                 upperbin < 200000 ~ "$150,000 -\n$200,000",
                                 upperbin == 200000 ~ "More than\n$200,000",
                                 TRUE ~ "total")
                                
         ) %>% 
  filter(income_group != "total") %>% 
  group_by(property, contour, income_group) %>% 
  summarise(estimate = sum(estimate)) %>% 
  mutate(percent = estimate/sum(estimate))
  ungroup() %T>%
  write_csv("local_data/hh_income.csv")

# education summary -------------------------------------------------------
education <- 
  acs_local_area("educational attainment") %>% 
  mutate(education_group = 
           case_when(
             str_detect(var_name, "grade|GED|Reg|Nur|No|Kin") == TRUE ~ "high school\nor less",
             str_detect(var_name, "Some|Asso") == TRUE ~ "associates or\nsome college",
             str_detect(var_name, "Bach") == TRUE ~ "undergraduate\ndegree",
             str_detect(var_name, "Mas|Doc|Prof") == TRUE ~ "postgrad or\nprofessional\ndegree",
             TRUE ~ "total")
  ) %>% 
  filter(education_group != "total") %>% 
  group_by(property, contour, education_group) %>% 
  summarise(estimate = sum(estimate)) %>% 
  mutate(percent = estimate/sum(estimate)) %>%
  ungroup() %T>%
  write_csv("local_data/education.csv")

# race/ethnicity -------------------------------------------------------------
race_ethnicity <- 
  acs_local_area("hispanic or latino origin by race") %>% 
  mutate(re_group = 
           case_when(
             str_detect(var_name, "excluding|including") == TRUE ~ "drop",
             str_detect(var_name, "^Hispanic or Latino(?=.)") == TRUE ~ "Hispanic/\nLatino",
             str_detect(var_name, "(?<=N.{22})Asian|(?<=N.{22})Native") == TRUE ~ "Asian/\nPacific\nIslander",
             str_detect(var_name, "(?<=N.{22})Black") == TRUE ~ "Black",
             str_detect(var_name, "(?<=N.{22})White") == TRUE ~ "White",
             str_detect(var_name, "(?<=N.{22})Some|(?<=N.{22})Two|(?<=N.{22})Ame") == TRUE ~ "Multiracial/\nOther",
             TRUE ~ "drop")
  ) %>% 
  filter(re_group != "drop") %>% 
  group_by(property, contour, re_group) %>% 
  summarize(estimate = sum(estimate)) %>%
  mutate(percent = estimate/sum(estimate)) %>% 
  ungroup()  %T>%
  write_csv("local_data/race_ethnicity.csv")


#total population -----------------------------------------------------------
population <- 
  acs_local_area("population") %>% 
  filter(var_name == "Total")  %T>%
  write_csv("local_data/total_population.csv") 
  

#renter_share ---------------------------------------------------------------
renter_share <- 
  acs_local_area("tenure") %>% 
  filter(var_name != "Total") %>% 
  group_by(property, contour) %>% 
  mutate(percentage = estimate/sum(estimate)) %>% 
  ungroup() %T>%
  write_csv("local_data/rentershare.csv")
    
rm(census_vars)  
save.image ("local_data/census.RData")

  

