library(tidyverse)
library(tidycensus)
library(sf)
library(viridis)
library(extrafont)
library(units)
library(ggmap)
library(scales)
library(ggrepel)
library(tigris)
library(leaflet)

tod_db <- read_csv('data/tod_database_download.csv')
distance <- set_units(.5, miles) %>% set_units(meters)

#   filter(grepl("COURTHOUSE", `Station Name`) == TRUE) %>%

tod_courthouse <- tod_db %>%
  filter(Buffer == "Existing Transit") %>%
  rename(lon = Longitude,
         lat = Latitude) %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) %>% st_transform(crs = 2248) 

tod_buffer <- tod_courthouse  %>%
  st_buffer(distance) %>%
  st_transform(2248) %>%
  st_transform("+init=epsg:2248 +proj=longlat +datum=WGS84") %>%
  st_union()

arlington_bg <-
  get_decennial(
    geography = "block group",
    state = "VA",
    variables = c("P0010001"),
    geometry = TRUE
  )

arlington_bg_2248 <- st_transform(arlington_bg, 2248) %>%
  st_transform("+init=epsg:2248 +proj=longlat +datum=WGS84")
arlington_tract_2248 <- st_transform(arlington_tract, 2248) %>%
  st_transform("+init=epsg:2248 +proj=longlat +datum=WGS84")

# Number of intersecting stations if we weren't doing a union.
# bgs_with_rail <- st_intersects(arlington_bg_2248, tod_buffer, sparse = FALSE)
# rail_vector <- bgs_with_rail %>% rowSums() > 0

hh_inc_2016_with_2248 <- st_transform(hh_inc_2016_with, 2248) %>%
  st_transform("+init=epsg:2248 +proj=longlat +datum=WGS84")

rail_vector <- st_intersects(hh_inc_2016_with_2248, tod_buffer, sparse = FALSE)

arlington_bg2 <- hh_inc_2016_with_2248 %>%
  mutate(rail = rail_vector)

arlington_bg2_nogeo <- arlington_bg2 %>%
  as.data.frame() %>%
  select(-geometry)

railandtype <- hh_inc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  left_join(arlington_bg2_nogeo, by = "GEOID")

railandtype %>%
  group_by(change) %>%
  summarize(`Tracts` = n(),
            `Tracts with rail` = sum(rail),
            `Percentage` = round(n()/sum(rail) * 100, 2)) %>%
  write_csv("output/typeandrail.csv")
