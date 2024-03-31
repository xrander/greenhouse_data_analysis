# Analysis -------------------------------------------------------
total_regions_monitored <- function(data) {
  length(unique(data$country))
}
library(leaflet.providers)

# Total emissions ---------------------------------------------------------

yearly_emission <- function(data, choice_year) {
  data |> 
    filter(year == choice_year) |> 
    summarize(
      tot_sum = sum(emission)
    )
}

total_emission <- function(data) {
  data |> 
    summarize(
      tot_emis = sum(emission)
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

leaflet() |> 
  addProviderTiles(provider = "NASAGIBS.ViirsEarthAtNight2012")

