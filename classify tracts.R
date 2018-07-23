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

hh_inc_2000_nospatial <- read_csv("data/LTDB_Std_2000_Sample.csv", guess_max = 2000) %>%
  select(GEOID = TRTID10, estimate_2000 = HINC00) %>%
  mutate(GEOID = as.character(GEOID))

us <- unique(fips_codes$state)[1:51]

hh_inc_2016 <- reduce(
  map(us, function(x) {
    get_acs(geography = "tract", variables = "B19013_001", 
            state = x, geometry = TRUE)
  }), 
  rbind
)

hh_inc_2016_msa_src <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                           variables = c("B01003_001", "B19013_001"), year = 2016)

hh_inc_2016_msa <- hh_inc_2016_msa_src %>%
  spread(variable, estimate) %>%
  group_by(GEOID) %>%
  summarize(NAME = first(NAME),
            population = min(B01003_001, na.rm = TRUE),
            median_2016 = min(B19013_001, na.rm = TRUE)) %>%
  arrange(desc(population)) %>%
  head(100) %>%
  rename(msa = GEOID)

metros <- core_based_statistical_areas(cb = TRUE)
metros_wash <- metros %>%
  select(metro_name = NAME,
         msa = GEOID) %>%
  right_join(hh_inc_2016_msa)

hh_inc_2016_with <- st_join(hh_inc_2016, metros_wash, join = st_within, 
                      left = FALSE)

hh_inc_2016_with <- hh_inc_2016_with %>%
  rename(estimate_2016 = estimate)

hh_inc <- hh_inc_2016_with %>%
  left_join(hh_inc_2016_msa, by = "msa") %>%
  left_join(hh_inc_2000_nospatial, by = "GEOID")

hh_inc_allmetros <- hh_inc %>%
  group_by(metro_name) %>%
  mutate(decile_2000 = ntile(estimate_2000, 10),
         decile_2016 = ntile(estimate_2016, 10),
         change_d = decile_2016 - decile_2000,
         change = cut(change_d, breaks = c(-Inf, -2, 2, Inf))) %>%
  ungroup()

hh_inc_allmetros$change <- hh_inc_allmetros$change %>%
  fct_recode("Declining" = "(-Inf,-2]",
                                   "Neither" = "(-2,2]",
                                   "Gentrifying" = "(2, Inf]")

hh_inc_allmetros <- hh_inc_allmetros %>%
  mutate(change = ifelse(decile_2000 > 4 & change == "Gentrifying", "Neither", as.character(change)),
         change = ifelse(decile_2016 > 4 & change == "Declining", "Neither", as.character(change)),
         change = as.factor(change))

hh_inc_allmetros$change %>% levels()

hh_inc_leaflet <- hh_inc_allmetros %>%
  filter(grepl("MD", metro_name)) %>%
  as("Spatial")

pal <- colorFactor(palette="viridis", domain = hh_inc_leaflet$change,
                          na.color="transparent")

labels <- sprintf(
  "<strong>%s</strong><br/>
  Median household income (2000 decile): %g<br/>
  Median household income (2016 decile): %g<br/>
  MSA: %s<br/>
  Median metro household income (2016): %g",
  hh_inc_leaflet$NAME.x, hh_inc_leaflet$decile_2000, hh_inc_leaflet$decile_2016,
  hh_inc_leaflet$metro_name, hh_inc_leaflet$median_2016.x
) %>% lapply(htmltools::HTML)


leaflet(data = hh_inc_leaflet) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(fillColor = ~pal(hh_inc_leaflet$change),
              stroke = FALSE, fillOpacity = .5,
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal, values = ~hh_inc_leaflet$change, opacity = 0.9,
            title = "Neighborhood type<br/>2000-2016", position = "bottomleft")

# Summary statistics

hh_inc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(metro_name) %>%
  summarize(gentrifying = sum(change == "Gentrifying", na.rm = TRUE),
            declining = sum(change == "Declining", na.rm = TRUE),
            neither = sum(change == "Neither", na.rm = TRUE),
            total = n()) %>%
  write_csv("output/tract_types.csv")

hh_inc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  summarize(gentrifying = sum(change == "Gentrifying", na.rm = TRUE),
            declining = sum(change == "Declining", na.rm = TRUE),
            neither = sum(change == "Neither", na.rm = TRUE),
            total = n())

hh_inc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(metro_name, change) %>%
  summarize(count = n()) %>%
  spread(change, count) %>%
  write_csv("output/tract_types.csv")
