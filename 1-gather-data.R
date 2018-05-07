library(tidyverse)
library(tidycensus)
library(purrr)

# Add Census key -- you will need to create this file yourself.
# census_api_key('your key here', install = TRUE)

source('keys.R')

counties <- fips_codes %>%
  filter(state_code < 60)

# Some special areas in fips_codes return errors
income_median <- map2_df(counties$state_code, counties$county_code, function(x, y) {
  result <- tryCatch(
    {
      get_decennial(year = 1990,
                geography = 'tract',
                variables = 'P080A001',
                state = x,
                county = y,
                cache_table = TRUE)
    },
    error = function(c) {
      df <- data.frame(stringsAsFactors = FALSE,
                       GEOID = paste0(x, y),
                       NAME = 'Error',
                       variable = 'P080A001',
                       value = NA)
      return(df)
    }
    )
})
