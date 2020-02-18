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

##################################### DATA #####################################

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

