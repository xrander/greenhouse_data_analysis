library("tidyverse")
library("janitor")

files <- list.files(pattern = "\\.csv$", full.names = T)

ghg_data <- map_df(files, read_csv) %>% 
  clean_names() %>% 
  select(-country_or_area_code)

skimr::skim(ghg_data)

ghg_data %>% 
  filter(series_code == "CH4") %>% 
  ggplot(aes(year, value, group = country_or_area))+
  geom_line(aes(col = country_or_area))+
  geom_area(aes(fill = country_or_area),
            alpha = 0.70)+
  theme_light()
  #theme(legend.position = "none")

ghg_data <- ghg_data %>%
  group_by(series_code, country_or_area) %>% 
  arrange(country_or_area, wt = year,
          .by_group = T) %>%
  nest() %>% 
  mutate(lagged_value = map(data, lag)) %>% 
  unnest(cols = c(data, lagged_value),
         names_sep = "_") %>% 
  select(c(1:4,6)) %>% 
  rename("gas" = series_code,
         "region" = country_or_area,
         "year" = data_year,
         "value" = data_value,
         "lagged_value" = lagged_value_value) %>% 
  mutate(percent_change = (value-lagged_value)/lagged_value * 100) %>% 
  ungroup() %>% 
  mutate_if(is.character, factor)

