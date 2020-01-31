# packages ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(httr)
library(R.utils)

# geospatial
library(sp)
library(sf)
library(ggmap)
library(geojsonsf)
library(mapview)

options(stringsAsFactors = FALSE)

time_a <- 5
time_b <- 7
time_c <- 10

# functions --------------------------------------------------------------

isochrone <- function (lonlat, time) {
  # mapbox api wrapper function
  
 parameters <- list(
   contours_minutes = time,
   polygons = "true",
   denoise = 1,
   access_token = Sys.getenv("mapbox_apikey") ## requires mapbox key
 )
    
 pull <- 
   GET(url = "https://api.mapbox.com",
       path = paste0("/isochrone/v1/mapbox/driving/", lonlat),
       query = parameters) %>% 
  
   content("text") %>%
   geojson_sf() %>% 
   select(contour, geometry) %>% 
   mutate(lonlat = lonlat)


return(pull)
}

join_culver <- function (time, df) {
  
union <-   
  st_union(
    df %>% 
      filter(property == "Culver City Senior Center" & contour == time),
    df %>% 
      filter(property == "Veterans Memorial Park" & contour == time)
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
  
  join <- st_join(x, y, join = st_intersects) %>% 
    filter(is.na(!!as.symbol(paste0(join_var, ".y"))) == FALSE) %>% 
    select(-!!as.symbol(paste0(join_var, ".y")))
  
  return(join)
}

# map charging stations --------------------------------------------------

# list of sites provided by evgo from EVgo
station_list_raw <- 
  read_csv("evgo_locations/site_list_20200123.csv") %>% 
  rename_all(tolower) %>% 
  rename_all(.funs = list(~ str_replace_all(., "\\s-?|/", "_")))


# only keep indicated sites and create additional variables
station_list <- 
  station_list_raw %>% 
  filter(include == 1) %>% 
  mutate(num_chargers = str_extract(layout, "^\\d") %>% as.numeric(),
         charger_kw = str_extract(layout, "\\d+(?=kw)") %>% as.numeric()
  ) %>% 
  select(property, address, num_chargers, charger_kw, location_description, type)


# use google geocode api to map station addresses
register_google(Sys.getenv("g_apikey"))

station_list_geocode <- 
  geocode(location = station_list$address) %>% ## google geocode api
  bind_cols(station_list, .)

station_geocode <- 
  station_list_geocode %>% 
  mutate(lonlat = paste0(lon,",",lat)) %>% 
  select(property, lonlat, type)

final_station_list <- 
  station_list_geocode %>% 
  
  # create culver city stations entry for geograpic center
  bind_rows(
    
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
  )


  

# convert geocoded lat lons to simple features
station_sf <- 
  station_list_geocode %>% 
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>%
  st_transform(32610)

# outputs ---------------------------------------------------------------------------
write_csv(station_list, "service_areas/station_list.csv")
st_write(station_sf, "service_areas/station_sf.geojson", delete_dsn = TRUE)
  


# create local isochrone set --------------------------------------------------------
#  create isochrone polygon and spatial crosswalk between that isochrone and
#  census tracts

isochrone_home <- 
  do.call(rbind,
          station_geocode$lonlat %>% 
          map(isochrone, time = paste(time_a, time_b, time_c, sep = ","))
  ) %>% 
  left_join(station_geocode,.) %>%
  st_as_sf() %>% 
  st_buffer(0) %>% 
  st_transform(32610)

# merge culver city sites 

culver_stations <- 
  map(c(5,7,10), join_culver, df = isochrone_home) %>% 
  do.call(rbind, .)
  

# final isochrone polys
isochrone_home_final <- 
  isochrone_home %>%
  filter(property != "Culver City Senior Center" & property != "Veterans Memorial Park") %>% 
  rbind(culver_stations)

# get census tract shapefiles if not available 
if (file.exists("shapefiles/ca_cts/cb_2018_06_tract_500k.shp") == FALSE) {
  
  download.file(url = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_06_tract_500k.zip",
                destfile = "cb_2018_06_tract_500k.zip")
  
  unzip("cb_2018_06_tract_500k.zip",
        exdir = "shapefiles/ca_cts")
  
  file.remove("cb_2018_06_tract_500k.zip")
}

# read in tract file
ca_tracts <-
  st_read("shapefiles/ca_cts/cb_2018_06_tract_500k.shp") %>% 
  st_transform(32610)

# calculate tract area
ca_tracts <- 
  ca_tracts %>%  
  mutate(tract_area = st_area(ca_tracts))

names(ca_tracts) <- names(ca_tracts) %>% tolower()

# create clipped tract shapefile
tracts_home_clip <- 
  st_intersection(ca_tracts, 
                  isochrone_home_final
  ) %>% 
  select(property, type, contour, geoid, tract_area)

#calculate area of tract within isochrone
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

mapview(local_service_area)

# outputs --------------------------------------------------------------
st_write(local_service_area, "service_areas/local_service_area.geojson", delete_dsn = TRUE)
st_write(isochrone_home_final, "service_areas/isochrone_polys.geojson", delete_dsn = TRUE)




  
# create commuteshed ---------------------------------------------------
#   identifies commute flows between workplaces around station and home
#   locations using LODES data. Creates a commutershed for commuting
#   baseline analysis.


# create commuteshed buffer 
buffer_commute <- 
  station_sf %>%
  st_buffer(1609.34 * 0.5) ## half a mile

# merge culver city site polygons
culver_stations_c <- 
  st_union(buffer_commute[18,],
           buffer_commute[19,]
  ) %>%
  
  select(-matches("\\.1")) %>% 
  mutate(property = "Culver City stations")

# final commute polygons
buffer_commute_final <- 
  buffer_commute[1:230,] %>% 
  rbind(culver_stations_c)

# get census block shapefile if not availble

if (file.exists("shapefiles/ca_blk/tl_2019_06_tabblock10.shp") == FALSE) {
  
  download.file(url = "https://www2.census.gov/geo/tiger/TIGER2019/TABBLOCK/tl_2019_06_tabblock10.zip",
                destfile = "tl_2019_06_tabblock10.zip")
  
  unzip("tl_2019_06_tabblock10.zip",
        exdir = "shapefiles/ca_blk")
  
  file.remove("tl_2019_06_tabblock10.zip")
}

# census block files
ca_blocks <-
  st_read("shapefiles/ca_blk/tl_2019_06_tabblock10.shp") %>% 
  st_transform(32610)

names(ca_blocks) <- names(ca_blocks) %>% tolower()

# join buffer to census polys to identify workplace area blocks
station_commute_blocks <- 
  st_join(x = buffer_commute_final,
          y = ca_blocks,
          join = st_intersects
  ) %>%
  as_tibble() %>% 
  select(property, geoid10)

station_commute_blocks <- 
  tibble(property = "UCLA - Lot 4", geoid10 = "060372655101002") %>% 
  bind_rows(station_commute_blocks)

# download LODES data if not available -------------------------------------------
if (file.exists("datasets/ca_od_main_JT01_2017.csv.gz") == FALSE) {
  
  download.file(url = "https://lehd.ces.census.gov/data/lodes/LODES7/ca/od/ca_od_main_JT01_2017.csv.gz",
                destfile = "datasets/ca_od_main_JT01_2017.csv.gz")
  }

lodes <- 
  read_csv("datasets/ca_od_main_JT01_2017.csv.gz") %>% 
    select(w_geocode, h_geocode, S000)

station_lodes <-
  left_join(station_commute_blocks,
            lodes,
            by = c("geoid10" = "w_geocode")
  ) %>%
  as_tibble() %>% 

  # extract tract geoid
  mutate(h_geoid = h_geocode %>% 
                   str_extract("^\\d{11}")
  ) %>% 
  
  # collapse on geoid
  group_by(property, h_geoid) %>% 
  summarize(commuters = sum(S000))

station_lodes_sf <- 
  station_lodes %>% 
  left_join(ca_tracts, by = c("h_geoid" = "geoid")) %>% 
  st_as_sf() %>% 
  select(property, h_geoid, commuters)

station_summary <- 
  station_lodes %>% 
  group_by(property) %>% 
  summarise(workers = sum(commuters, na.rm = TRUE))

rm(lodes) # save memory

# commute data from NHTS
if (file.exists("datasets/trippub.csv") == FALSE) {
  
  download.file("https://nhts.ornl.gov/assets/2016/download/Csv.zip",
                destfile = "datasets/nhts_data.zip")
  
  unzip("datasets/nhts_data.zip", files = "trippub.csv", exdir = "datasets")
  file.remove("datasets/nhts_data.zip")
}

nhts <- 
  read_csv("datasets/trippub.csv") %>% 
  rename_all(tolower) %>% 
  select(hhstate, hh_cbsa, trippurp, vmt_mile, wttrdfin) %>% 
  filter(hhstate == "CA" & trippurp == "HBW" & vmt_mile > 0)

ca_metro_codes <- 
  # from NHTS codebook
  tibble(hh_cbsa = c(31080, 40140, 40900, 41740, 41860, 41940, "XXXXX"),
         metro = c("Los Angeles-Long Beach-Anaheim",
                   "Riverside-San Bernardino-Ontario",
                   "Sacramento-Roseville-Arden-Arcade",
                   "San Diego-Carlsbad",
                   "San Francisco-Oakland-Hayward",
                   "San Jose-Sunnyvale-Santa Clara",
                   "Other")
  )

nhts_metro_90 <- 
  left_join(ca_metro_codes, nhts) %>% 
  group_by(metro, hh_cbsa) %>% 
  summarize(per90 = quantile(vmt_mile, .9)) %>% 
  ungroup()

# get cbsa shapefile
if (file.exists("shapefiles/cbsa/cb_2018_us_cbsa_20m.shp") == FALSE) {
  download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_cbsa_20m.zip",
                destfile = "shapefiles/cbsa/cb_2018_us_cbsa_20m.zip")
  
  unzip("shapefiles/cbsa/cb_2018_us_cbsa_20m.zip", exdir = "shapefiles/cbsa")
  file.remove("shapefiles/cbsa/cb_2018_us_cbsa_20m.zip")
}

cbsa <- st_read("shapefiles/cbsa/cb_2018_us_cbsa_20m.shp") %>% 
  rename_all(tolower) %>% 
  select(cbsafp, affgeoid, name) %>% 
  st_transform(32610)

commute_max_sf <- 
  st_join(station_sf, cbsa, join = st_intersects) %>% 
  left_join(nhts_metro_90, ., by = c("hh_cbsa" = "cbsafp")) %>% 
  filter(is.na(property) == FALSE) %>% 
  st_as_sf()

commute_buffer <- 
  commute_max_sf %>% 
  st_buffer(commute_max_sf$per90 * 1609.34)

# merge culver city site polygons
culver_stations_c <- 
  st_union(commute_buffer[5,],
           commute_buffer[6,]
  ) %>%
  
  select(-matches("\\.1")) %>% 
  mutate(property = "Culver City stations")

# final commute polygons
commute_buffer_final <- 
  commute_buffer %>%
  filter(property != "Culver City Senior Center" &
         property != "Veterans Memorial Park" ) %>% 
  rbind(culver_stations_c) %>% 
  select(property)

# clip commute
commuter_service_area <- 
  map(commute_buffer_final$property, 
      vector_st_join, 
      join_var = "property",
      data_x = station_lodes_sf,
      data_y = commute_buffer_final) %>% 
  do.call(rbind, .) 

# outputs -----------------------------------------------------------------------
st_write(commuter_service_area, "service_areas/commuter_service_area.geojson", delete_dsn = TRUE)

# service areas mapping ---------------------------------------------------------

rm(list=ls()[! ls() %in% c("local_service_area","commuter_service_area", "station_sf", "station_list_geocode", "final_station_list")])
save.image(file = "service_areas.RData")
         

