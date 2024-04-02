# Analysis -------------------------------------------------------
total_regions_monitored <- function(data) {
  length(unique(data$country))
}

# Total emissions ---------------------------------------------------------

yearly_emission <- function(data, my_year) {
  data |> 
    filter(year == my_year) |> 
    summarize(
      tot_sum = sum(emission)
    )
}

total_emission <- function(data) {
  data |> 
    summarize(
      tot_emis = round(sum(emission)/1000000, 1)
    )
}




# Top emitting Countries --------------------------------------------------

top_5_region_emission <- function(data, filter_year, filter_gas) {
  data |> 
    filter(year %in% {{ filter_year }} & gas %in% {{ filter_gas }}) |> 
    summarize(
      .by = c(country),
      tot_emission = sum(emission)
    ) |> 
    slice_max(tot_emission, n = 5)
}


plot_top_emitter <- function(top_emission_data) {
  top_emission_data |> 
    ggplot(aes(fct_reorder(country, tot_emission), tot_emission, fill = tot_emission)) +
    geom_col() +
    geom_label(
      aes(label = floor(tot_emission)),
      col = "blue", fill = "gray") +
    labs(x = "Country", y = "Emission (GGCO2e)") +
    coord_flip() +
    scale_y_continuous(label = label_comma()) +
    scale_fill_gradient(low = "lightblue4", high = "gray2") +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

total_emis_by_country <- function(data) {
  data |> 
    summarize(
      .by = c(year, country),
      emission_per_pop = mean(emission_per_pop)
    )
}


plot_emis_per_person <- function(data, country_var) {
  total_emis_by_country(data) |> 
    filter(country == {{ country_var }}) |> 
    ggplot(aes(year, emission_per_pop)) +
    geom_col(width = .1, fill = "tomato") +
    geom_point(
      size = 4, col = "darkgreen", fill = "tomato3",
      shape = "circle fill", stroke = 1.5
    ) +
    labs(x = "year", y = "Tons per person") +
    scale_x_continuous(breaks = seq(1990, 2020, 4))
}
  

plot_emis_per_person(emis_tbl, unique(emis_tbl$country)[4])



  

# emis_tbl |> 
#   filter(year == 2020) |> 
#   ggplot(aes(population, per_capital, size = emission, col = emission)) +
#   geom_point() +
#   labs(col = "Emission", size = "Emission") +
#   #scale_x_log10() +
#   #scale_y_log10() +
#   scale_color_viridis_c(direction = 1)
# 
# 
# emis_tbl |> 
#   summarize(
#     .by = c(gas, year),
#     percent_change = mean(change_in_emission)
#   ) |> 
#   filter(year %in% c(1991, 1992)) |> 
#   pivot_wider(
#     names_from = gas,
#     values_from = percent_change
#   ) |> 
#   filter(!is.na(CO2)) |> 
#   print(n = 100)
# 

# leaflet() |> 
#   addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012")

