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

################################# FUNCTIONS #################################### 

acs_local_area <- function (varlist, stations = local_service_area) {
## Passes variable names through to acs api to download acs tables. Then joins
## that data to census tracts within service areas based on geoid. Last uses
## allocation fraction to proportionally allocate values to the in-service-area
## portion of the tract.
  
    census_table <- 
        get_acs(geography = "tract",
                variables = pull(varlist, variable), 
                state = "CA") %>%
        ## rejoin variable labels
        left_join(varlist) %>% 
        rename_all(tolower) %>% 
        select(geoid, label, estimate)

    joined_census_table <- 
        left_join(stations, census_table) %>% 
        st_drop_geometry()

    allocated_census_data <- joined_census_table %>%      
        mutate(est_alloc = (allocation * estimate) %>%
                           round(digits = 0)
        ) %>% 
        group_by(property, contour, label) %>% 
        summarise(estimate = sum(est_alloc)) %>% 
        ungroup()
  
  return(allocated_census_data)

}

##################################### DATA #####################################

load("data/processed/image-files/service-areas.RData")

#################################### SCRIPT ####################################

## Passes sets of variables to the acs_local_area function and then processes
## the output for reporting

# census varlist----------------------------------------------------------------

## load and clean census variable list.
census_vars <- load_variables(2017, "acs5") %>% 
    mutate(label = label %>%
                   str_remove("Estimate\\!\\!(Total\\!\\!)?") %>% 
                   str_replace_all("\\!?\\!", " ") %>% 
                   str_replace_all("(?<=\\d)\\s(?=\\d)", ",") %>% 
                   str_replace("(or more)", "+")
    ) %>% 
    rename(variable = name)


# MUD unit summary -------------------------------------------------------------

## MUD counts pulled from census table b25024 - Units by number of units in
## structure. Structures with more than two unit per building are MUDs duplexes
## are excluded due to their similarities with SFRs.

## call API with B25024 table
units_buildings_raw <- 
    census_vars %>% 
    filter(str_detect(census_vars$variable, "B25024")) %>% 
    acs_local_area()

## discard data for non-mud buildings 
units_buildings <- units_buildings_raw %>%      
    filter(str_detect(label, "[3-9]"))
  
# income summary ---------------------------------------------------------------

## Household income stats pulled from census table B19001. Household income is
## binned into four categories: less than $75k, $75-150K, $150-200k, more than
## $200k

## call API using the B19001 table
hh_income_raw <- census_vars %>% 
    filter(str_detect(census_vars$variable, "B19001")) %>% 
    acs_local_area()

## rebin income categories
hh_income_rebinbin <- hh_income_raw %>%    
    mutate(upperbin = label %>%
                      str_remove_all("( \\+)|\\$|,") %>%
                      str_extract("\\d{5,6}$") %>% 
                      as.numeric(),

           income_group = case_when(
               upperbin <   75000 ~ "less than\n$75,000",
               upperbin <  150000 ~ "$75,000 -\n$150,000",
               upperbin <  200000 ~ "$150,000 -\n$200,000",
               upperbin == 200000 ~ "More than\n$200,000",
                             TRUE ~ "total")
                                
     )

## collapse into new income groups and calcualte percentage     
hh_income <- hh_income_rebin %>% 
    filter(income_group != "total") %>% 
    group_by(property, contour, income_group) %>% 
    summarise(estimate = sum(estimate)) %>% 
    mutate(percent = estimate/sum(estimate))
    ungroup()
    
# education summary ------------------------------------------------------------

## Education stats pulled from census table B15003. Educational attainment is
## rebined into collapsed categories

## call api with B15003 table    
education_raw <- census_vars %>%  
      filter(str_detect(census_vars$variable, "B15003")) %>% 
      acs_local_area()

## rebin variables using regex        
education_rebin
    mutate(education_group = case_when(
               str_detect(label, 
                          "grade|GED|Reg|Nur|No|Kin") ~ "high school\nor less",
               str_detect(label,
                          "Some|Asso") ~ "associates or\nsome college",
               str_detect(label,
                          "Bach") ~ "undergraduate\ndegree",
               str_detect(label, 
                          "Mas|Doc|Prof") ~ "postgrad or\nprofessional\ndegree",
               TRUE ~ "total"
             )
  )

## collapse into new education groups and calcualte percentage 
education <- education_rebin %>%     
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

  

