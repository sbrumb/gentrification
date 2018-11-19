library(sf)
library(geojsonio)
library(tidyverse)
library(USAboundaries)

us_can_stations <- geojson_read("data/us_can_stations.geojson",
                                what = "sp")
us_can_stations_street <- geojson_read("data/us_can_stations_street.geojson",
                                       what = "sp")

# Use 50 states and DC only
us_stations_sf <- us_can_stations %>%
  st_as_sf() %>%
  filter(!grepl(" (PR|AB|BC|MB|NB|NL|NT|NS|NU|ON|PE|QC|SK|YT|MX|DR)$", Region),
         !grepl("Panama PA", Region))

# TODO: Remove Canadian stations
us_stations_street_sf <- us_can_stations_street %>%
  st_as_sf() %>%
  filter(!grepl("Mexicable", Line)) %>%
  mutate(Region = NA)

us_stations_all_sf <- rbind(us_stations_sf, us_stations_street_sf)

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- do.call(rbind,sf::st_geometry(x))
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

us_stations_all <- sfc_as_cols(us_stations_all_sf, names = c("lon", "lat")) %>%
  as_data_frame() %>%
  select(-geometry)
  
us_stations_all %>%
  write_csv("output/stations.csv")

states_map <- us_states() %>%
  filter(!(name %in% c("Alaska", "Hawaii", "Puerto Rico")))

ggplot() +
  geom_sf(data = states_map, aes(fill = "#ddfffff")) +
  geom_sf(data = us_stations_street_sf)
