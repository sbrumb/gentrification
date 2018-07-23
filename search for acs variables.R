library(tidycensus)
library(tidyverse)

census00 <- load_variables(year = 2000, dataset = "sf3", cache = TRUE)
census00 %>%
  filter(grepl("ncome", label)) %>%
  select(name, label)

acs15 <- load_variables(2016, "acs5", cache = TRUE)
acs15 %>%
  filter(grepl("opulation", label)) %>%
  View()
