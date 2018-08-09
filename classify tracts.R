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
uspop2016 <- 318558162

hh_inc_2000_nospatial <- read_csv("data/LTDB_Std_2000_Sample.csv",
                                  guess_max = 2000) %>%
  select(GEOID = TRTID10, estimate_2000 = HINC00) %>%
  mutate(GEOID = as.character(GEOID))

us <- unique(fips_codes$state)[1:51]

hh_inc_2016 <- reduce(
  map(us, function(x) {
    get_acs(geography = "tract", variables = c("B01003_001", "B19013_001"),
            state = x, geometry = TRUE)
  }),
  rbind
)


hh_inc_2016_msa_src <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
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

hh_inc_2010 <- reduce(
  map(us, function(x) {
    get_acs(geography = "tract", variables = c("B19013_001"),
            state = x, year = 2010)
  }),
  rbind
)

hh_inc_2010 <- hh_inc_2010 %>%
  rename(estimate_2010 = estimate) %>%
  select(-NAME, -variable, -moe)

metros <- core_based_statistical_areas(cb = TRUE)
metros_join <- metros %>%
  select(metro_name = NAME,
         msa = GEOID) %>%
  right_join(hh_inc_2016_msa)

hh_inc_2016_with <- st_join(hh_inc_2016, metros_join, join = st_within,
                      left = FALSE)

hh_inc_2016_with2 <- hh_inc_2016_with %>%
  spread(variable, estimate) %>%
  rename(estimate_2016 = B19013_001,
         population_2016 = B01003_001) %>%
  group_by(GEOID) %>%
  summarize(msa = first(msa),
            name = first(NAME.x),
            metro_name = first(metro_name),
            estimate_2016 = estimate_2016[which(!is.na(estimate_2016))[1]],
            population_2016 = population_2016[which(!is.na(population_2016))[1]])

# hh_inc <- hh_inc_2016_with2 %>%
#   left_join(hh_inc_2016_msa, by = "msa") %>%
#   left_join(hh_inc_2000_nospatial, by = "GEOID")

hh_inc <- hh_inc_2016_with2 %>%
  left_join(hh_inc_2016_msa, by = "msa") %>%
  left_join(hh_inc_2010, by = "GEOID")


# hh_inc_allmetros <- hh_inc %>%
#   group_by(metro_name) %>%
#   mutate(decile_2000 = ntile(estimate_2000, 10),
#          decile_2016 = ntile(estimate_2016, 10),
#          change_d = decile_2016 - decile_2000,
#          change = cut(change_d, breaks = c(-Inf, -2, 2, Inf))) %>%
#   ungroup()

hh_inc_allmetros <- hh_inc %>%
  group_by(metro_name) %>%
  mutate(decile_2010 = ntile(estimate_2010, 10),
         decile_2016 = ntile(estimate_2016, 10),
         change_d = decile_2016 - decile_2010,
         change = cut(change_d, breaks = c(-Inf, -2, 2, Inf))) %>%
  ungroup()

hh_inc_allmetros$change <- hh_inc_allmetros$change %>%
  fct_recode("Declining" = "(-Inf,-2]",
                                   "Stable" = "(-2,2]",
                                   "Gentrifying" = "(2, Inf]")

# hh_inc_allmetros <- hh_inc_allmetros %>%
#   mutate(change = ifelse(decile_2000 > 4 & change == "Gentrifying",
#                          "Upgrading", as.character(change)),
#          change = ifelse(decile_2016 > 4 & change == "Declining",
#                          "Downgrading", as.character(change)),
#          change = ifelse(decile_2000 <= 4 & decile_2016 <= 4 &
#                            change == "Stable",
#                          "Stable low-income", as.character(change)),
#          change = as.factor(change))

hh_inc_allmetros <- hh_inc_allmetros %>%
  mutate(change = ifelse(decile_2010 > 4 & change == "Gentrifying",
                         "Upgrading", as.character(change)),
         change = ifelse(decile_2016 > 4 & change == "Declining",
                         "Downgrading", as.character(change)),
         change = ifelse(decile_2010 <= 4 & decile_2016 <= 4 &
                           change == "Stable",
                         "Stable low-income", as.character(change)),
         change = as.factor(change))

hh_inc_allmetros$change %>% levels()

hh_inc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(change) %>%
  summarize(count = n(),
            population = sum(population_2016),
            uspop_pct = percent_format()(population/uspop2016)) %>%
  View()

# Summary statistics

hh_inc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(metro_name, change) %>%
  summarize(count = n()) %>%
  spread(change, count)

hh_inc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(metro_name) %>%
  summarize(gentrifying = sum(change == "Gentrifying", na.rm = TRUE),
            declining = sum(change == "Declining", na.rm = TRUE),
            upgrading = sum(change == "Upgrading", na.rm = TRUE),
            downgrading = sum(change == "Downgrading", na.rm = TRUE),
            stable = sum(change == "Stable", na.rm = TRUE),
            stableli = sum(change == "Stable low-income", na.rm = TRUE),
            total = n()) %>%
  write_csv("output/tract_types.csv")

