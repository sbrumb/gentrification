library(tidyverse)

tod <- read_csv("data/tod_database_download.csv")

tod %>%
  filter(Buffer == "Existing Transit") %>%
  group_by(`Year Opened`) %>%
  summarize(count = n())
