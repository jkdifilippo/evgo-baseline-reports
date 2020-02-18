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

## set options
options(stringsAsFactors = FALSE)

## set google api key for ggmap (secret)
register_google(Sys.getenv("g_apikey"))

################################ FUNCTIONS #####################################

isochrone <- function (lonlat, time) {
## mapbox api wrapper function. Passes lat/lon and travel time vars to mapbox
## api and returns simple feature objects (polys) for each time interval
    parameters <- 
        list(contours_minutes = time,
               polygons = "true",
               denoise = 1,
               access_token = Sys.getenv("mapbox_apikey") ## secret key
        )
    pull <- GET(url = "https://api.mapbox.com",
                path = paste0("/isochrone/v1/mapbox/driving/", lonlat),
                query = parameters
            ) %>% 
        content("text") %>%
        geojson_sf() %>% 
        select(contour, geometry) %>% 
        mutate(lonlat = lonlat)
  
    return(pull)
}
  
join_culver <- function (time, df) {
## one off function that combines two proximate culver city stations into one
      
    union <- 
        st_union(df %>% 
                 filter(property == "Culver City Senior Center" &
                        contour == time),
                 df %>% 
                 filter(property == "Veterans Memorial Park" &
                        contour == time)
        ) %>% 
        select(-matches("\\.1")) %>% 
        mutate(property = "Culver City stations")
    
    return(union)  
}
  
vector_st_join <- function (join_val, join_var, data_x, data_y) {
    x <- 
        data_x %>% 
        filter(!!as.symbol(join_var) == join_val)
    
    y <- 
        data_y %>% 
        filter(!!as.symbol(join_var) == join_val)
    
    join <- 
        st_join(x, y, join = st_intersects) %>% 
        filter(is.na(!!as.symbol(paste0(join_var, ".y"))) == FALSE) %>% 
        select(-!!as.symbol(paste0(join_var, ".y")))
    
    return(join)
  }
  
############################# ANALYSIS OPTIONS #################################

## travel time variables define the set of isochrone polylines for  
travel_time_min <- 5
travel_time_central <- 7
travel_time_max <- 10

############################### DATA/INPUTS ####################################
  
# list of sites provided by EVgo -----------------------------------------------

station_list_raw <- 
    read_csv("data/raw/evgo-locations/site-list.csv") %>% 
    rename_all(tolower) %>% 
    rename_all(list(~ str_replace_all(., "\\s-?|/", "_")))

# get census tract shapefiles from census.gov ----------------------------------

ctract_source <- 
    "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_06_tract_500k.zip"

ctract_path <- 
    "data/raw/shapefiles/cb_2018_06_tract_500k.shp"

## download file only if not already in directory
if (file.exists(ctract_path) == FALSE) {
  
    download.file(url = ctract_source,
                  destfile = "data/cb_2018_06_tract_500k.zip")
   
    unzip("data/cb_2018_06_tract_500k.zip",
          exdir = "data/raw/shapefiles")
    
    file.remove("data/cb_2018_06_tract_500k.zip")
}

## read census tract file
ca_tracts <-
  st_read(ctract_path) %>% 
  st_transform(32610)

################################## SCRIPT ######################################

# process site list ------------------------------------------------------------

## keep only indicated sites and create site characteristic variables
station_list <- 
    station_list_raw %>% 
    filter(include == 1) %>% 
    mutate(num_chargers = str_extract(layout, "^\\d") %>% as.numeric(),
           charger_kw = str_extract(layout, "\\d+(?=kw)") %>% as.numeric()
    ) %>% 
    select(property,
           address,
           num_chargers,
           charger_kw,
           location_description,
           type)
  
## Geocode station list addresses. If geocoded file already exists, skip
## geocoding and read in geocoded file, saving on gmap api calls on repeadted
## runs. *file must be deleted manually if station list changes*

geocode_file_path <- "data/processed/station-list-geocode.csv"

if (file.exists(geocode_file_path)) {
    station_list_geocode <- read_csv(geocode_file_path)
} else {
    
    station_list_geocode <- 
        geocode(location = station_list$address) %>% 
        bind_cols(station_list, .)
    
    write_csv(station_list_geocode, 
              geocode_file_path)
}

## Veterans Memorial Park and Culver City Senior Center are less than 1/2 mile
## from each other, making them functionally the same site from a baseline stats
## perspective. This joins the two by creating a Culver City Stations entry with
## a center-point that is the geograpic center between the two adjacent station
## sites

culver_city_station <- 
    tibble(property = "Culver City stations",
           address = "Culver City, California",
           num_chargers = 0,
           charger_kw = 0,
           location_description = "multiple",
           lon = station_list_geocode %>% 
                 filter(property %in% c("Veterans Memorial Park", 
                                        "Culver City Senior Center")) %>% 
                 pull(lon) %>% 
                 mean(),
           lat = station_list_geocode %>% 
                 filter(property %in% c("Veterans Memorial Park",
                                        "Culver City Senior Center")) %>% 
                 pull(lat) %>% 
                 mean()
      )

## add culver city station back to stations list
station_list_processed <-
    bind_rows(station_list_geocode,
              culver_city_station)

## convert geocoded lat/lon to simple features
  station_sf <- station_list_geocode %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(32610)


# create local service area isochrone set --------------------------------------

## format station lat/lon data so that it can be passed through to mapbox api
station_lonlat <- station_list_geocode %>%
    mutate(lonlat = paste0(lon,",",lat)) %>%
    select(property, lonlat, type)

## define station isochrone polygons using mapbox api call and joun properties.
## Will produce errors, this is normal
station_isochrones <- 
    map_df(station_lonlat$lonlat,
           isochrone,
           time = paste(travel_time_min,
                        travel_time_central, 
                        travel_time_max, 
                        sep = ",")
    )

station_isochrones_joined <- 
    left_join(station_lonlat, station_isochrones) %>%
    unique() %>% 
    st_as_sf(crs = 4326) %>% 
    st_buffer(0) %>% 
    st_transform(32610)

  
## merge culver city service area polygons. This creates combined polygons where
## travel times are set for ability to reach either station
culver_stations <- 
    map(c(5,7,10), join_culver, df = station_isochrones_joined) %>%
    do.call(rbind, .)

## drop separate culver city sites and add in merged station
station_isochrones_final <- 
    station_isochrones_joined %>%
    filter(!property %in% c("Culver City Senior Center",
                            "Veterans Memorial Park")
    ) %>% 
    rbind(culver_stations)

# calculate fraction of tracts within service area -----------------------------

## Census tract boundaries do not perfectly overlap isochrone boundaries. To
## approximate demographic characteristics of area within service areas, we use
## proportional allocation which allocates values in proportion to how much area
## of a census tract falls inside the service area.

## calculate total tract area
ca_tract_area <- 
    ca_tracts %>%  
    mutate(tract_area = st_area(ca_tracts)) %>% 
    rename_all(tolower)

## clip tracts to isochrones
tracts_home_clip <- 
    st_intersection(ca_tract_area, 
                    station_isochrones_final
    ) %>% 
    select(property, type, contour, geoid, tract_area)

# calculate area of tract within isochrone (clipped area). Census tracts where
# less than 10% of the tract area falls within service area are removed to
# reduce error risk
local_service_area <- 
    tracts_home_clip %>% 
    mutate(clip_area =  st_area(tracts_home_clip ) %>% 
                        str_replace_all(" \\[m^2\\]", "") %>% 
                        as.numeric(),
           
           tract_area = tract_area %>%
                        str_replace_all(" \\[m^2\\]", "") %>% 
                        as.numeric(),
             
           allocation = round(clip_area/tract_area,
                              digits = 2)
    ) %>% 
    filter(allocation > 0.1)
  
################################### OUTPUTS ###################################

## clear unneccessary objects from memory
rm(list=ls()[! ls() %in% c("local_service_area",
                           "station_list_geocode",
                           "final_station_list",
                           "station_sf"
                           )])

## save image file for use in later analyses
save.image("data/processed/image-files/service-areas.RData")
  
    
################################### CLEANUP ###################################
rm(list=ls())
         

