library(tidyverse)
library(tidycensus)
library(totalcensus)
library(sf)

us_fips <- unique(fips_codes$state)[1:51]
us_tracts <- as_tibble()

options(tigris_use_cache = TRUE)

for (state_fips in us_fips) {
  state_tracts <- tracts(state = state_fips)
  state_tracts_sf <- st_as_sf(state_tracts)
  rbind(us_tracts, state_tracts_sf)
}

us_tracts <- reduce(
  map(us_fips, function(x) {
    tracts(state = x)
  }),
  rbind
)

us_tracts2 <- st_as_sf(us_tracts)

us_tracts2 %>%
  write_rds('output/us_tracts.rds')

test <- read_rds('output/us_tracts.rds')
