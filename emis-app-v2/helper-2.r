# Comparison Section ---------------------------------------------------------
## Row 1 ---------------------------------------------------------------
total_emis_by_country <- function(data, country_var) {
  data |> 
    summarize(
      .by = c(year, country),
      emission_per_pop = mean(emission_per_pop)
    ) |> 
    filter(country %in% {{country_var}})
}


plot_emis_per_person <- function(data) {
  data |> 
    ggplot(aes(year, emission_per_pop)) +
    geom_col(width = .1, fill = "tomato") +
    geom_point(
      size = 4, col = country, fill = country,
      shape = "circle fill", stroke = 1.5
    ) +
    labs(x = "year", y = "Tons per person") +
    scale_x_continuous(breaks = seq(1990, 2020, 4))
}


plot_emis_per_person(emis_tbl, c(unique(emis_tbl$country)[4:5]))

total_emis_by_country(emis_tbl, c("United States of America", "Russia", "United Kingdom")) |> 
  ggplot(aes(year, emission_per_pop, fill = country, col = country)) +
  geom_col(
    position = position_dodge(width = .2),
    width = .1
  )  +
  geom_point(
    position = position_dodge(width = .2),
    shape = "circle fill", stroke = 1.5, size = 2
  ) +
  scale_fill_tableau() +
  scale_color_tableau() +
  geom_label(
    aes(x = 2010, y = .01, label = list(unique(country)), col = "black")
  )
