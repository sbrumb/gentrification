library(tidyverse)
library(tidycensus)
library(totalcensus)
library(sf)
library(viridis)
library(extrafont)
library(units)
library(ggmap)
library(scales)
library(ggrepel)
library(tigris)
library(leaflet)

options(tigris_use_cache = TRUE)

tod_distance <- set_units(.5, miles) %>% set_units(meters)

tod_db <- read_csv('data/tod_database_download.csv')

tod_sb <- read_csv('data/stations 2011 onward.csv') %>%
  filter(year >= 2012,
         year <= 2015) %>%
  select(-Mode, -url, -notes, -region) 
  
tod_db2 <- tod_db %>%
  filter(Buffer == "Existing Transit") %>%
  rename(year = `Year Opened`,
         lon = Longitude,
         lat = Latitude,
         name = `Station Name`,
         lines = `Line(s)`) %>%
  select(-Buffer, -Agency)

tod_existing <- tod_db2 %>%
  mutate(year = ifelse(year == "Pre-2000", 2000, year),
         year = as.numeric(year)) %>%
  rbind(tod_sb) %>%
  filter(year <= 2013) %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) %>% st_transform(crs = 2248) 

tod_buffer <- tod_existing  %>%
  st_buffer(tod_distance) %>%
  st_transform(2248) %>%
  st_transform("+init=epsg:2248 +proj=longlat +datum=WGS84") %>%
  st_union()

us_fips <- unique(fips_codes$state)[1:51]
us_tbl <- as_tibble()

for (state_fips in us_fips) {
  state_bg <- block_groups(state = state_fips)
  state_bg_sf <- st_as_sf(state_bg)
  rm(state_bg)
  
  state_bg_2248 <- st_transform(state_bg_sf, 2248) %>%
    st_transform("+init=epsg:2248 +proj=longlat +datum=WGS84")
  rm(state_bg_sf)
  
  rail_vector <- st_intersects(state_bg_2248, tod_buffer, sparse = FALSE)
  
  state_tbl <- as_tibble(cbind(state_bg_2248$GEOID, rail_vector)) %>%
    rename(GEOID = V1,
           rail_halfmile = V2)
  
  us_tbl <- rbind(us_tbl, state_tbl)
}

# 11:17 PM

us_tbl %>%
  mutate(year = 2013) %>%
  write_csv("output/bg_stations_2013.csv")
