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
library(stringr)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

source('keys.R')

# TODO: Add and use 'year' variable to simplify updates

uspop2000 <- 281421906
uspop2015 <- 321039839 # Census 2017 population estimates

hhinc_2000_ltdb <- read_csv("data/LTDB_Std_2000_Sample.csv",
                                  guess_max = 2000) %>%
  select(GEOID = TRTID10, est_2000 = HINC00) %>%
  mutate(GEOID = str_pad(as.character(GEOID), 11, pad = "0"))

us_fips <- unique(fips_codes$state)[1:51]

hhinc_2015 <- reduce(
  map(us_fips, function(x) {
    get_acs(geography = "tract", variables = c("B01003_001", "B19013_001"),
            state = x, year = 2015, geometry = TRUE)
  }),
  rbind
)

hhinc_2015_msa_src <- get_acs(
  geography = "metropolitan statistical area/micropolitan statistical area",
  variables = c("B01003_001", "B19013_001"), year = 2015)

hhinc_2015_msa <- hhinc_2015_msa_src %>%
  spread(variable, estimate) %>%
  group_by(GEOID) %>%
  summarize(NAME = first(NAME),
            population = min(B01003_001, na.rm = TRUE),
            msa_median_2015 = min(B19013_001, na.rm = TRUE)) %>%
  arrange(desc(population)) %>%
  head(100) %>%
  rename(msa = GEOID)

cbsas <- core_based_statistical_areas(cb = TRUE)

hhinc_2015_spatial <- cbsas %>%
  select(metro_name = NAME,
         msa = GEOID) %>%
  right_join(hhinc_2015_msa)

# TODO: Check warning message: "although coordinates are longitude/latitude,
# st_within assumes that they are planar"
hhinc_2015_within <- st_join(hhinc_2015, hhinc_2015_spatial, join = st_within,
                      left = FALSE)

hhinc_2015_within2 <- hhinc_2015_within %>%
  spread(variable, estimate) %>%
  rename(est_2015 = B19013_001,
         pop_2015 = B01003_001) %>%
  group_by(GEOID) %>%
  summarize(msa = first(msa),
            name = first(NAME.x),
            metro_name = first(metro_name),
            est_2015 = est_2015[which(!is.na(est_2015))[1]],
            pop_2015 = pop_2015[which(!is.na(pop_2015))[1]])

hhinc_allyears <- hhinc_2015_within2 %>%
  left_join(hhinc_2015_msa, by = "msa") %>%
  left_join(hhinc_2000_ltdb, by = "GEOID")

hhinc_allmetros <- hhinc_allyears %>%
  group_by(metro_name) %>%
  mutate(decile_2000 = ntile(est_2000, 10),
         decile_2015 = ntile(est_2015, 10),
         change_d = decile_2015 - decile_2000,
         type = cut(change_d, breaks = c(-Inf, -2, 2, Inf))) %>%
  ungroup()

# TODO: Refactor next two commands
hhinc_allmetros$type <- hhinc_allmetros$type %>%
  fct_recode("Declining" = "(-Inf,-2]",
                                   "Stable" = "(-2,2]",
                                   "Gentrifying" = "(2, Inf]")

# Expand upon Landis (2015) by distinguishing between gentrifying/upgrading,
# declining/downgrading, and stable/stable low-income
hhinc_allmetros <- hhinc_allmetros %>%
  mutate(type = ifelse(decile_2000 > 4 & type == "Gentrifying",
                         "Upgrading", as.character(type)),
         type = ifelse(decile_2015 > 4 & type == "Declining",
                         "Downgrading", as.character(type)),
         type = ifelse(decile_2000 <= 4 & decile_2015 <= 4 &
                           type == "Stable",
                         "Stable low-income", as.character(type)),
         type = as.factor(type))

# What percentage of the population lives in each type? (This is for the
# 100 largest metropolitan areas.)

usmetropop2015 <- sum(hhinc_allmetros$pop_2015)

# TODO: Investigate NAs  
hhinc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(type) %>%
  summarize(count = n(),
            population = sum(pop_2015),
            uspop_pct = (population/usmetropop2015)) %>%
  mutate(uspop_pct = percent(uspop_pct, accuracy = .1)) %>%
  View()

# Tabulations by metropolitan area
hhinc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(metro_name, type) %>%
  summarize(count = n()) %>%
  spread(type, count, fill = 0) %>%
  write_csv("output/tracts_msa.csv")

# Census tracts
hhinc_allmetros %>%
  as.data.frame() %>%
  select(-geometry) %>%
  write_csv("output/classification.csv")
