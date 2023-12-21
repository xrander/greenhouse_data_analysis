setwd("~/Documents/Data Science/Personal Project/ghg_data_analysis/")
library("plotly")
library("tidyverse")
library("janitor")


files <- list.files(pattern = "\\.csv$", full.names = T)
files <- files[-c(1,2)]


ghg_data <- map_df(files, read_csv) %>% 
  clean_names() %>% 
  select(-country_or_area_code)


skimr::skim(ghg_data)

ghg_data %>% 
  ggplot(aes(series_code))+
  geom_bar()
  #theme(legend.position = "none")

ghg_data <- ghg_data %>% 
  mutate_if(is.character, factor)


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
  ungroup()

write.csv(ghg_data_long, "ghg_pivot_longer.csv")

ghg_data_long %>% 
  filter(gas %in% c("N20", "CO2", "MIX", "CH4") & region %in% c("United States of America", "United Kingdom")) %>% 
  filter(year >= 1999 & year <= 2019) %>% 
  group_by(region, gas) %>% 
  summarize(total = sum(value)) %>% 
  pivot_wider(names_from = gas,
              values_from = total) %>% 
  rowwise() %>% 
  mutate(emistotal = sum(c_across(-1), na.rm = T))

na_to_zero <- function(x){
  ifelse(is.na(x), 0, x)
} # easily change all nas to 0

ghg_data_wide <- ghg_data_long %>% 
  select(gas, region, year, value) %>% 
  pivot_wider(names_from = gas,
              values_from = c(value),
              values_fn = mean) %>%
  mutate(across(-c(1:2), na_to_zero))

un_population <- read_csv("population_data.csv",
                          col_types = list("Country or Area" = col_character(),
                                           "Year" = col_double(),
                                           "Area" = col_character(),
                                           "Sex" = col_character(),
                                           "Record Type" = col_character(),
                                           "Value" = col_double(),
                                           "Value Footnotes" = col_double()
                                           )
                          )



un_population <- un_population %>%
  select(`Country or Area`, Sex, Year, Area, Value) %>% 
  filter(Sex == "Both Sexes" & Area == "Total") %>% 
  select(c(1,3,5)) %>% 
  rename("region" = "Country or Area",
         "year" = "Year",
         "population" = "Value")

ghg_data_wide_sample <- ghg_data_wide %>% 
  left_join(un_population, join_by(region, year)) %>% 
  relocate(region, year, population)


ggplotly(ghg_data_wide_sample %>% 
           filter(year == 2010) %>% 
           ggplot(aes(CH4, CO2, size = population))+
           geom_point(aes(col = region))+
           scale_x_log10() +
           scale_y_log10()+
           theme_minimal()+
           theme(legend.position = "none"))


ghg_data_long %>%
  filter(region %in% c("Australia","Canada") & gas %in% c("CH4", "CO2")) %>% 
  filter(year == 1999) %>% 
  ggplot(aes(region, value, fill = gas))+
  geom_col()+
  theme_ipsum_es(grid = "Y")+
  scale_fill_viridis(discrete = T)+
  scale_y_continuous(label = scales::comma)

ghg_data_long
