library(tidyverse)

tod_db2 <- read_csv('data/tod_database_download.csv')

tod_db2 <- tod_db2 %>%
  rename(year = `Year Opened`) 

tod_db2 %>%
  group_by(year) %>%
  summarize(count = n())
