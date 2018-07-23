library(tidyverse)
library(tidycensus)
library(sf)
library(viridis)
library(extrafont)
library(units)
library(ggmap)
library(scales)
library(ggrepel)

tod_dc <- read_csv('data/tod_database_download.csv')
distance <- set_units(.5, miles) %>% set_units(meters)

tod_courthouse <- tod_dc %>%
  filter(grepl("COURTHOUSE", `Station Name`) == TRUE) %>%
  rename(lon = Longitude,
         lat = Latitude) %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) %>% st_transform(crs = 2248) 

tod_buffer <- tod_courthouse  %>%
  st_buffer(distance) %>%
  st_transform(4326)


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

arlington_bg_2248 <- st_transform(arlington_bg, 2248)
arlington_tract_2248 <- st_transform(arlington_tract, 2248)

ggplot() +
  geom_sf(
    data = arlington_bg,
    color = "#FA9E3BFF",
    fill = NA,
    size = .5,
    inherit.aes = FALSE,
    show.legend = "line"
  ) +
  geom_sf(
    data = arlington_tract,
    color = "#BD3786FF",
    fill = NA,
    size = 1,
    inherit.aes = FALSE,
    show.legend = "line"
  ) +
  geom_sf(
    data = tod_buffer,
    color = NA,
    fill = "#00000011",
    inherit.aes = FALSE,
    show.legend = "polygon"
  ) +
  geom_sf(
    data = tod_courthouse,
    inherit.aes = FALSE
  ) +
  scale_shape_manual(values = c(19)) +
  coord_sf() +
  scale_x_continuous(limits = c(-77.11, -77.06)) +
  scale_y_continuous(limits = c(38.88, 38.902)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank())

