library(tidyverse)
library(tidycensus)
library(purrr)
library(sf)
library(tigris)
library(viridis)
library(ggspatial)
library(ggmap)
library(leaflet)
library(scales)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

source('keys.R')

# hh_inc_2009 <- get_acs(geography = "tract", variables = "B19013_001", 
#                        state = c("DC", "VA", "MD"), geometry = FALSE, year = 2009)
# 
# hh_inc_2000 <- get_decennial(geography = "tract", variables = "P053001", 
#                  state = c("DC", "VA", "MD"), geometry = FALSE, year = 2000)

hh_inc_2000_nospatial <- read_csv("data/LTDB_Std_2000_Sample.csv", guess_max = 2000) %>%
  select(GEOID = TRTID10, estimate_2000 = HINC00) %>%
  mutate(GEOID = as.character(GEOID))

hh_inc_2016 <- get_acs(geography = "tract", variables = "B19013_001E", 
                       state = c("DC", "VA", "MD"), geometry = TRUE, year = 2016)

hh_inc <- hh_inc_2016 %>%
  left_join(hh_inc_2000_nospatial) %>%
  mutate(estimate_2016 = estimate)

metros <- core_based_statistical_areas(cb = TRUE)
metros_wash <- metros %>%
  filter(NAME %in% c("Washington-Arlington-Alexandria, DC-VA-MD-WV",
                     "Baltimore-Columbia-Towson, MD",
                     "Richmond, VA")) %>%
  select(metro_name = NAME)

hh_inc_dmv <- st_join(hh_inc, metros_wash, join = st_within, 
                             left = FALSE)

hh_inc_dmv <- hh_inc_dmv %>%
  group_by(metro_name) %>%
  mutate(decile_2000 = ntile(estimate_2000, 10),
         decile_2016 = ntile(estimate_2016, 10),
         median_2000 = median(estimate_2000, na.rm = TRUE),
         median_2016 = median(estimate_2016, na.rm = TRUE),
         change_d = decile_2016 - decile_2000,
         change = cut(change_d, breaks = c(-Inf, -2, 2, Inf))) %>%
  ungroup()

hh_inc_dmv$change <- hh_inc_dmv$change %>%
  fct_recode("Declining" = "(-Inf,-2]",
                                   "Neither" = "(-2,2]",
                                   "Gentrifying" = "(2, Inf]")

hh_inc_dmv <- hh_inc_dmv %>%
  mutate(change = ifelse(decile_2000 > 4 & change == "Gentrifying", "Neither", as.character(change)),
         change = ifelse(decile_2016 > 4 & change == "Declining", "Neither", as.character(change)),
         change = as.factor(change))

hh_inc_change$change %>% levels()

dmv_geom <- hh_inc_dmv %>%
  as("Spatial")

pal <- colorFactor(palette="plasma", domain = dmv_geom$change,
                          na.color="transparent")

labels <- sprintf(
  "<strong>%s</strong><br/>
  Median household income (2000 decile): %g<br/>
  Median household income (2016 decile): %g<br/>
  Median metro household income (2000): %g<br/>
  Median metrohousehold income (2016): %g",
  dmv_geom$NAME, dmv_geom$decile_2000, dmv_geom$decile_2016,
  dmv_geom$median_2000, dmv_geom$median_2016
) %>% lapply(htmltools::HTML)

leaflet(data = dmv_geom) %>%
  addTiles() %>%
  addPolygons(fillColor = ~pal(dmv_geom$change),
              stroke = FALSE, fillOpacity = .5,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = ~dmv_geom$change, opacity = 0.9,
            title = "Neighborhood type<br/>2000-2016", position = "bottomleft")

