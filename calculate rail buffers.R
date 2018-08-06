library(tidyverse)
library(tidycensus)
library(sf)
library(viridis)
library(extrafont)
library(units)
library(ggmap)
library(scales)
library(ggrepel)
library(leaflet)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

tod_dc <- read_csv('data/tod_database_download.csv')
distance <- set_units(.5, miles) %>% set_units(meters)

#   filter(Buffer == "Existing Transit") %>%


tod_courthouse <- tod_dc %>%
  filter(grepl("COURTHOUSE", `Station Name`) == TRUE) %>%
  rename(lon = Longitude,
         lat = Latitude) %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 2248,
    remove = FALSE
  ) %>% st_transform(crs = 2248)

tod_buffer <- tod_courthouse %>%
  st_buffer(distance) %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 2248,
    remove = FALSE
  ) %>% st_transform(crs = 2248)

arlington_bg <-
  get_decennial(
    geography = "block group",
    county = "Arlington",
    state = "VA",
    variables = c("P0010001"),
    geometry = TRUE
  )

arlington_tract <-
  get_decennial(
    geography = "tract",
    county = "Arlington",
    state = "VA",
    variables = c("P0010001"),
    geometry = TRUE
  )

arlington_bg_2248 <- st_transform(arlington_bg, 2248) %>%
  st_transform("+init=epsg:2248 +proj=longlat +datum=WGS84")
arlington_tract_2248 <- st_transform(arlington_tract, 2248) %>%
  st_transform("+init=epsg:2248 +proj=longlat +datum=WGS84")

arlington_bg_2248 <- st_transform(arlington_bg, 2248)
arlington_tract_2248 <- st_transform(arlington_tract, 2248)

bgs_with_rail <- st_intersects(arlington_bg_2248, tod_buffer, sparse = FALSE)

rail_vector <- as.logical(rowSums(bgs_with_rail))

arlington_bg2 <- arlington_bg %>%
  mutate(rail = rail_vector)
  
labels <- sprintf(
  "<strong>%s</strong><br/>
  Intersects: %g<br/>",
  arlington_bg2$GEOID,
  arlington_bg2$rail
) %>% lapply(htmltools::HTML)

leaflet(data = arlington_bg2) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addProviderTiles(providers$OpenMapSurfer.AdminBounds) %>%
  addPolygons(label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))

leaflet(data = tod_buffer) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  addProviderTiles(providers$OpenMapSurfer.AdminBounds) %>%
  addPolygons()
