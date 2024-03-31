
# Load Package ------------------------------------------------------------

source("data/dependencies.R")

base_url <- paste("https://data.un.org/ws/rest/data/UNSD,DF_UNData_UNFCC,1.0/.EN_ATM_METH_XLULUCF+",
                  "EN_ATM_CO2E_XLULUCF+EN_CLC_GHGE_XLULUCF+EN_ATM_HFCE+",
                  "EN_ATM_NO2E_XLULUCF+EN_ATM_PFCE+EN_ATM_SF6E.AUS+AUT+BLR+BEL+BGR+CAN+HRV+",
                  "CYP+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+JPN+LVA+",
                  "LIE+LTU+LUX+MLT+MCO+NLD+NZL+NOR+POL+PRT+ROU+RUS+SVK+SVN+ESP+SWE+CHE+TUR+UKR+GBR+USA+EU1./",
                  "ALL/?detail=full&dimensionAtObservation=TIME_PERIOD",
                  sep = "")
# Connecting to API to support auto updates
res <- GET(base_url,
           add_headers(""),
           accept("text/csv"))

content <- content(res, type = "text", encoding = "utf-8")

ghg_data <- read_csv(content, col_types = cols()) |> 
  clean_names()

ghg_data <- ghg_data |> 
  mutate(
    indicator = str_remove_all(indicator, pattern = "._XLULUCF$"),
    indicator = str_remove_all(indicator, pattern = "(EN_ATM_|EN_CLC_)")
  )

ghg_data <- ghg_data |> 
  left_join(
    countrycode::codelist |>  select(country.name.en, genc3c),
    join_by(ref_area == genc3c)
  ) |> 
  select(country.name.en, indicator, time_period, obs_value) |> 
  rename(country = country.name.en) |> 
  mutate(
    country = case_when(
      is.na(country) ~ "European Union",
      country == "Turkey" ~ "Turkiye",
      country == "United States" ~ "United States of America",
      .default = country
  ),
  country = str_trim(country, side = "both")
  )


format_large_number <- function(x) {
  if(x >= 1e12) {
    return(paste(format(round(x/1e12), nsmall = 1), " Trillion"))
  } else if (x >=1e9) {
    return(paste(format(round(x/1e9), nsmall = 1), "Billion"))
  } else if (x >=1e6) {
    return(paste(format(round(x/1e6), nsmall = 1), "Million"))
  } else if (x >=1e3) {
    return(paste(format(round(x/1e3), nsmall = 1), "Thousand"))
  } else {
    return(as.character(x))
  }
}


# Metrics needed for bubble chart
population_data <- read_csv(
  "https://raw.githubusercontent.com/xrander/greenhouse_data_analysis/master/data/population_data.csv",
  col_types = list("Country or Area" = col_character(),
                   "Year" = col_double(),
                   "Area" = col_character(),
                   "Sex" = col_character(),
                   "Record Type" = col_character(),
                   "Value" = col_double(),
                   "Value Footnotes" = col_double()
  )
)


un_pop <- population_data |> 
  select(`Country or Area`, Sex, Year, Area, Value) |> 
  filter(Sex == "Both Sexes" & Area == "Total") |> 
  select(c(1,3,5)) |> 
  rename("country" = "Country or Area",
         "year" = "Year",
         "population" = "Value") |> 
  mutate(
    year = as.integer(year),
    country = case_when(
      country == "Russian Federation" ~ "Russia",
      country == "Türkiye" ~ "Turkiye",
      country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      country != "United Kingdom of Great Britain and Northern Ireland" ~ country
      ),
    country = str_trim(country, side = "both")
  )

european_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
                        "Estonia", "Finland", "France", "Germany", "Greece",
                        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                        "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                        "Spain", "Sweden") # EU is not represented as a data point.

eu_pop <- un_pop |> 
  filter(country %in% european_countries) |> 
  mutate(is_eu = "European Union") |> 
  group_by(year, is_eu) |> 
  summarize(population = sum(population)) |> 
  rename("country" = is_eu) |> 
  relocate(country)

un_pop <- un_pop |> 
  bind_rows(eu_pop) |> 
  arrange(country, year)

# The same will be repeated for the GDP Per Capital

per_capital_data <-read_csv("https://raw.githubusercontent.com/xrander/greenhouse_data_analysis/master/data/gdp.csv") |> 
  rename(country = region) |> 
  mutate(
    country = case_when(
      country == "United States" ~ "United States of America",
      country == "Russian Federation" ~ "Russia",
      country == "Türkiye" ~ "Turkiye",
      country == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
      country != "United States" ~ country
    ),
    country = str_trim(country, side = "both")
  )

eu_per_capital <- per_capital_data|> 
  filter(country %in% european_countries) |> 
  mutate(
    is_eu = "European Union"
  ) |> 
  group_by(year, is_eu) |> 
  summarize(per_capital = mean(gdp)) |> 
  rename("country" = is_eu) |> 
  relocate(country)


un_per_capital <- per_capital_data |> 
  rename("per_capital" = "gdp") |> 
  bind_rows(eu_per_capital) |> 
  arrange(country, year)

un_data <- un_pop |> 
  right_join(un_per_capital, join_by(country, year))

un_data <- un_data |> 
  group_by(country, year) |> 
  summarize(population = mean(population, na.rm = T),
            per_capital = mean(per_capital, na.rm = T))

# The data

ghg_data_merged <- ghg_data |> 
  left_join(un_data, join_by(country == country, time_period == year)) |> 
  rename(
    year = time_period,
    gas = indicator,
    emission = obs_value
  )

ghg_data_merged <- ghg_data_merged |> 
  filter(country != "Monaco")

## Further processing -------------------------------------------------------------------
glimpse(ghg_data_merged)

# ghg_data_merged |> 
#   plot_time_series(
#     year, emission,
#     .facet_vars = gas,
#     .color_var = country,
#     .smooth = FALSE,
#     .facet_ncol = 3,
#     
#   ) # Remove all countries within the EU

ghg_data_merged <- ghg_data_merged |> 
  filter(!country %in% european_countries)


ghg_data_merged <- ghg_data_merged |> 
  mutate(
    emission_per_capital = emission/per_capital,
    emission_per_pop = emission/population,
    change_in_emission = (emission - lag(emission))/emission * 100
  )


write_csv(ghg_data_merged, "data/data.csv")


# Modeling ----------------------------------------------------------------

mod_tbl <- ghg_data_merged |> 
    summarize(
      .by = c(year, gas),
      emission = sum(emission)
    ) |> 
    mutate(
      year = make_date(year = year, month = 12, day = 31),
      gas = factor(gas)
    )

mod_tbl_nested <- mod_tbl |> 
  extend_timeseries(
    .id_var = gas,
    .date_var = year,
    .length_future = 20
  ) |> 
  nest_timeseries(
    .id_var = gas,
    .length_future = 20,
    .length_actual =  32
  ) |> 
  split_nested_timeseries(
    .length_test = 6
  )

## Feature engineering ----------------------------------------------------

mod_rec <- recipe(emission ~ ., data = extract_nested_train_split(mod_tbl_nested))  |> 
  step_timeseries_signature(year) |> 
  step_rm(matches("(_month)|(_wday)|(week)|(year_)")) |> 
  step_zv()

mod_rec2 <- mod_rec |> 
  update_role(year, new_role = "ID")

## Model Specification --------------------------------------------------

est_model <- exp_smoothing() |> 
  set_mode("regression") |> 
  set_engine("ets") 

prop_mod <- prophet_reg() |> 
  set_mode("regression") |> 
  set_engine("prophet")

rand_mod <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("regression")

prop_boost_mod <- prophet_boost() |> 
  set_mode("regression") |> 
  set_engine("prophet_xgboost")

lm_mod <- linear_reg() |> 
  set_mode("regression") |> 
  set_engine("lm")

## Model Workflow --------------------------------

est_wf <- workflow() |> 
  add_recipe(mod_rec) |> 
  add_model(est_model)

prop_wf <- workflow() |> 
  add_recipe(mod_rec) |> 
  add_model(prop_mod)

rand_wf <- workflow() |> 
  add_recipe(mod_rec2) |> 
  add_model(rand_mod)

prop_bst_wf <- workflow() |> 
  add_recipe(mod_rec) |> 
  add_model(prop_boost_mod)

lm_wf <- workflow() |> 
  add_model(lm_mod) |> 
  add_recipe(mod_rec)


mod_fit <- modeltime_nested_fit(
  nested_data = mod_tbl_nested,
  est_wf, 
  prop_wf,
  rand_wf,
  prop_bst_wf,
  lm_wf
)


extract_nested_error_report(mod_fit)

mod_fit |> 
  extract_nested_test_accuracy() |> 
  pivot_longer(
    cols = mae:rsq,
    names_to = "eval_metric",
    values_to = "error"
  ) |> 
  filter(!is.na(error)) |> 
  ggplot(aes(fct_reorder(gas, error), error, fill = eval_metric)) +
  geom_col(position = "dodge") +
  facet_wrap(~eval_metric, scales = "free_x") +
  coord_flip()


## Extract best model ------------------------------------------------------


mod_best <- mod_fit |> 
  modeltime_nested_select_best(metric = "mae")

mod_best |> 
  extract_nested_best_model_report()


# mod_best |> 
#   extract_nested_test_forecast() |> 
#   group_by(gas) |> 
#   plot_modeltime_forecast(
#     .facet_ncol = 2,
#     .legend_show = FALSE,
#     .conf_interval_show = FALSE
#   )


# model refit -------------------------------------------------------------

mod_refit <- mod_best |> 
  modeltime_nested_refit()

saveRDS(mod_refit, "data/forecast_mod.rds")


# Remove all other objects ------------------------------------------------

rm(list = ls())
