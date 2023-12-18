library("tidyverse")
library("janitor")


files <- list.files(pattern = "\\.csv$", full.names = T)
files <- files[-1]


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

ghg_data_long <- ghg_data %>%
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

write.csv(ghg_data_long, "emission/ghg_pivot_longer.csv")


ghg_data_wide <- ghg_data_long %>% 
  select(gas, region, year, value) %>% 
  pivot_wider(names_from = year,
              values_from = c(value),
              values_fn = mean)


ghg_data_long %>% 
  filter(gas %in% c("N20", "CO2", "MIX", "CH4") & region %in% c("United States of America", "United Kingdom")) %>% 
  filter(year >= 1999 & year <= 2019) %>% 
  group_by(region, gas) %>% 
  summarize(total = sum(value)) %>% 
  pivot_wider(names_from = gas,
              values_from = total) %>% 
  rowwise() %>% 
  mutate(emistotal = sum(c_across(-1), na.rm = T))


ghg_data_wide <- ghg_data_long %>% 
  select(gas, region, year, value) %>% 
  pivot_wider(names_from = year,
              values_from = c(value),
              values_fn = mean)

ghg_data_long %>% 
  filter(gas %in% c("CH4", "N2O") & region %in% c("United Kingdom", "Canada", "Belgium")) %>% 
  filter(year >= 1990 & year <= 2019) %>% 
  group_by(region, gas) %>%
  summarize(total = sum(value)) %>% 
  pivot_wider(names_from = gas,
              values_from = total) %>% 
  rowwise() %>% 
  ncol()
