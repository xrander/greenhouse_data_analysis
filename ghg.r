library("tidyverse", "tidymodels","vip")


files <- list.files(pattern = "\\.csv$", full.names = T)

ghg_data <- map_dfr(files, read_csv)

skimr::skim(ghg_data)

