# Overview Section -------------------------------------------
## Row 1 -------------------------------------------------

total_regions_monitored <- function(data) {
  length(unique(data$country))
}


yearly_emission <- function(data, my_year) {
  data |> 
    filter(year == my_year) |> 
    summarize(
      tot_sum = round(sum(emission), 2)
    ) |> 
    mutate(tot_sum = paste0(ceiling(tot_sum/1000000), "M GGCO2e"))
}


total_emission <- function(data) {
  data |> 
    summarize(
      tot_emis = round(sum(emission)/1e9, 1)
    )
}


total_emission(emis_tbl)

percent_change_year <- function(data, filter_year) {
  data |> 
    summarize(
      .by = c(year),
      total_emission = sum(emission)
    ) |> 
    mutate(
      last_emission = lag(total_emission),
      percent_change = round((total_emission - last_emission)/last_emission * 100, 2),
      status = case_when(total_emission > last_emission ~ "bad",
                         .default = "good")
    ) |> 
    drop_na(percent_change) |> 
    filter(year == {{ filter_year }})
}




## Row 2 --------------------------------------------------

### Element data for plot--------------------------------------------
percent_change_country <- function(data, filter_year) {
  data |> 
    summarize(
      .by = c(year, country),
      total_emission = sum(emission)
    ) |> 
    group_by(country) |> 
    mutate(
      last_emission = lag(total_emission),
      percent_change = round((total_emission - last_emission) / last_emission * 100, 2),
      status = case_when(percent_change > 0 ~ "bad",
                         .default = "good")
    ) |> 
    ungroup() |> 
    filter(year == {{ filter_year }})
}

top_5_region_emission <- function(data, filter_year) {
  data |> 
    filter(year == {{ filter_year }}) |> 
    summarize(
      .by = c(year, country),
      tot_emission = sum(emission)
    ) |> 
    slice_max(tot_emission, n = 5)
}

least_5_emitter <- function(data, filter_year) {
  data |> 
    filter(year == {{ filter_year }}) |> 
    summarize(
      .by = c(year, country),
      tot_emission = sum(emission)
    ) |> 
    slice_min(tot_emission, n = 5)
}

### Plot element -----------------------------------------------------
plot_yearly_change <- function(data) {
  plot_ly(
    data, x = ~country, y = ~round(percent_change, 1)/ 100,
    color = ~status, type = "bar", showlegend = FALSE, colors = "YlOrRd"
  ) |> 
    layout(
      title = list(
        text = paste0("Percentage Change of Monitored Regions in Year ", unique(data$year)),
        font = list(size = 18)
      ),
      xaxis = list(title = "", tickangle = 320, tickfont = list(size = 10)),
      yaxis = list(title = "", tickformat = "1%")
    )
}

plot_top_emitter <- function(data) {
  data |> 
    ggplot(aes(tot_emission, fct_reorder(country, tot_emission), fill = tot_emission)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(
      low = "green", mid = "gray", 
      high = muted("red"), labels = comma
    ) +
    geom_label(
      aes(label = paste0(round(tot_emission/1e6, 2), "M")),
      fill = "white",
      col = muted("red")
    ) +
    scale_x_continuous(
      labels = label_comma(),
      breaks = seq(min(data$tot_emission), max(data$tot_emission), 10000000)
    )  +
    theme_dark() +
    theme(legend.position = "none") +
    labs(
      x = "", y = "",
      fill = "Total Emission",
      title = paste0("Top Emitting Countries for year ", unique(data$year))
    )
}


plot_least_emitter <- function(data) {
  data |> 
    ggplot(aes(tot_emission, fct_reorder(country, tot_emission, .fun = min), fill = tot_emission)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(
      low = "gray", mid = "white", 
      high = muted("springgreen"), labels = comma
    ) +
    geom_label(
      aes(label = paste0(round(tot_emission/1e6, 2), "M")),
      fill = "white",
      col = muted("springgreen")
    ) +
    scale_x_continuous(
      labels = label_comma(),
      breaks = seq(min(data$tot_emission), max(data$tot_emission), 100000)
    )  +
    theme_dark() +
    theme(legend.position = "none") +
    labs(
      x = "", y = "",
      fill = "Total Emission",
      title = paste0("Least Emitting Countries for year ", unique(data$year))
    )
}

## Row 3 ---------------------------------------------------------------------

gas_prop <- function(data, filter_year) {
  data |> 
    filter(year == {{ filter_year }} ) |> 
    summarize(
      .by = c(gas, year),
      total_emission = sum(emission)
    )
}

plot_gas_prop <- function(gas_prop_data) {
  dt |> 
    plot_ly(
      labels = ~gas, values = ~ total_emission,
      type = "pie", textposition = "inside", textinfo = "label+percent",
      insidetextfont = list(color = "white", size = 15), hoverinfo = "text",
      text = ~paste0(round(total_emission/1000000, 2), " Million GGCO2e"),
      hole = .7, showlegend = FALSE
    ) |> 
    layout(
      title =  paste0("Proportion of Gas Emission for ", unique(gas_prop_data$year)),
      font = list(size = 13),
      paper_bgcolor = "transparent"
    )
}

#### Look for matches
# emis_country <- emis_tbl |> 
#   mutate(
#     country = case_when(country == "United States of Americ" ~ "United States",
#                         country == "Russia" ~ "Russian Federation",
#                         country == "Turkiye" ~ "Turkey",
#                         .default = country)
#   )
# 
# world <- spData::world
# 
# world_ln_lt <- map_data("world")
# 
# 
# european_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
#                         "Estonia", "Finland", "France", "Germany", "Greece",
#                         "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
#                         "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
#                         "Spain", "Sweden")
# 
# map_tbl <- world |>
#    select(name_long, continent, area_km2) |>
#    mutate(
#      name_long = case_when(name_long %in% european_countries ~ "European Union",
#                            .default = name_long)
#    ) |>
#  left_join(emis_country, join_by(name_long == country), relationship = "many-to-many")
# 
# 
# map_tbl <- sf::st_drop_geometry(map_tbl)
# 
# 
# 
# 
# 
# map_tbl <- map_tbl |>
#    summarize(
#      .by = c(name_long, year, geom),
#      emission = sum(emission),
#      population = mean(population),
#      per_capital = mean(per_capital),
#      emission_per_capital = mean(emission_per_capital),
#      emission_per_pop = mean(emission_per_pop)
#    ) |> 
#   rename(country = name_long)
# 
# 
# maptbl2 <- sf::st_transform(map_tbl, 4326)
# 
# 
# 
# maptbl2 |> 
#   filter(year == 2010) |> 
#   leaflet() |> 
#   addTiles() |>
#   addPolygons(
#     fillColor = ~ emission,
#     stroke = NA,
#     color = "#E84A5F"
#   )
# 
# basemap <- list(
#   scope = "world",
#   showland = TRUE,
#   landcolor = toRGB("#e5ecf6"),
#   showcountries = TRUE,
#   resolution = 140
# )
# 
# plot_ly(type = "scattergeo", mode = "markers") |> 
#   layout(geo = basemap)
# 
# 
# library(mapproj)
# map_data("world")
