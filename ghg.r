# Load libraries
library("tidyverse")
library("plotly")
library("hrbrthemes")
library("viridis")
library("janitor")
library("scales")
library("httr")
library("jsonlite")


base_url <- paste0("https://data.un.org/ws/rest/data/UNSD,DF_UNData_UNFCC,1.0/",
                   ".EN_ATM_METH_XLULUCF+EN_ATM_CO2E_XLULUCF+EN_CLC_GHGE_XLULUCF",
                   "+EN_ATM_HFCE+EN_ATM_NO2E_XLULUCF+EN_ATM_PFCE+EN_ATM_SF6E.AUS+",
                   "AUT+BLR+BEL+BGR+CAN+HRV+CYP+CZE+DNK+EST+FIN+FRA+DEU+GRC",
                   "+HUN+ISL+IRL+ITA+JPN+LVA+LIE+LTU+LUX+MLT+MCO+NLD+NZL+NOR+",
                   "POL+PRT+ROU+RUS+SVK+SVN+ESP+SWE+CHE+TUR+UKR+GBR+USA+EU1./ALL/",
                   "?detail=full&dimensionAtObservation=TIME_PERIOD",
                   sep = "")
# Connecting to API to support auto updates

res <- GET(base_url,
           add_headers(""),
           accept("text/csv"))

content <- content(res, type = "text", encoding = "utf-8")

ghg_data <- read_csv(content, col_types = cols()) |> 
  clean_names()

gas_formula <- tibble(
  indicator = unique(ghg_data$indicator),
  gas = c("CO2", "HFC", "CH4", "NO2", "PFC", "SF6", "GHG")
)


ghg_data <- ghg_data |> 
  left_join(gas_formula, join_by(indicator)) |> 
  relocate(ref_area, gas, .before = dataflow) |> 
  select(1:2, 7:8)

countrycode::guess_field(ghg_data$ref_area)
# genc3c column in country code is a good match

ghg_data_countries <- ghg_data |> 
  left_join(countrycode::codelist |> select(country.name.en, genc3c),
             join_by("ref_area" == "genc3c"))

ghg_data_countries <- ghg_data_countries |>
  relocate(country.name.en, .after = ref_area) |> 
  rename(
    "region_abbr" = ref_area,
    "region" = country.name.en,
    "year" = time_period,
    "emission_value" = obs_value
  )
  

unique(is.na(ghg_data_countries)) #region is having missing values

ghg_data_countries |> 
  count(region, region_abbr) |> 
  arrange(desc(is.na(region)))


greenhouse_gas <- ghg_data_countries |> 
  mutate(region = case_when(is.na(region) ~ "European Union",
                            .default = region)) |>
  mutateif(is_character, factor)




skimr::skim_without_charts(ghg_data)

ghg_data |> 
  group_by(region, gas) |> 
  count(gas)

ghg_data |>
  group_by(gas) |> 
  summarize(number = n()) |> 
  ggplot(aes(fct_reorder(gas, number), number, fill = gas))+
  geom_col()+
  theme_ipsum_es()+
  coord_flip()

ghg_data |> 
  group_by(year, gas) |> 
  summarize(total_emissions = sum(emission_value)) |> 
  pull(total_emissions) |> 
  sum() |>
  round(1) |> 
  paste0(" KCO2e", sep = " ")# First table result for dashboard 1


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

ghg_data |> 
  filter(year %in% c(1990:2000) & gas == c("CO2", "CH4")) |> 
  group_by(gas) |> 
  summarize(total_emission = sum(emission_value)) |> 
  pull(total_emission) |> 
  sum() |> 
  format_large_number()


  
ghg_data |>
  filter(year == 2020) |> # to be filtered by year
  group_by(gas) |>
  summarize(emission = sum(emission_value)) |>
  mutate(gas_proportion = round(emission/sum(emission) * 100, 1),
         ylab_pos = cumsum(gas_proportion) - 0.35 * gas_proportion) |> 
  plot_ly(labels = ~gas, values = ~gas_proportion, type = "pie")

#  second plot, dashboard 1 result,
# should only filter by year as some of the gases emission are too low and disrupts the visualization,
# they are therefore collapsed.

ghg_data |> 
  # to be filtered by year and gas
  group_by(region) |> 
  summarize(total_emission = sum(emission_value)) |> 
  arrange(desc(total_emission)) |> 
  top_n(n = 10, wt = total_emission) # result 3 to be used for dashboard 1, title: top_emitting_countries. 
  

ghg_data |> 
  filter(year %in% c(1990, 2020)) |>  # slider year is input for shiny
  ggplot(aes(year, emission_value, col = gas))+
  geom_line()+
  #geom_area(aes(fill = gas), alpha = 0.7)+
  #scale_y_log10()+
  scale_y_continuous(labels = scales::comma)# 4th result to be used for dashboard 1


ghg_data |> 
  select(year, gas, emission_value) |> 
  group_by(year, gas) |> 
  summarize(emis = sum(emission_value)) |> 
  ggplot(aes(year, emis, group = gas, col = gas))+
  geom_line()

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


un_population <- un_population |>
  select(`Country or Area`, Sex, Year, Area, Value) |> 
  filter(Sex == "Both Sexes" & Area == "Total") |> 
  select(c(1,3,5)) |> 
  rename("region" = "Country or Area",
         "year" = "Year",
         "population" = "Value")


eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
        "Estonia", "Finland", "France", "Germany", "Greece",
        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
        "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
        "Spain", "Sweden") # EU is not represented as a data point.
                          #list will be created to include EU in the data point

eu_pop <- un_population |> 
  filter(region %in% eu_countries) |> 
  mutate(is_eu = "European Union") |> 
  group_by(year, is_eu) |> 
  summarize(population = sum(population)) |> 
  rename("region" = is_eu) |> 
  relocate(region)

un_population <- un_population |> 
  bind_rows(eu_pop) |> 
  mutate(region = ifelse(region == "United Kingdom of Great Britain and Northern Ireland",
                         "United Kingdom", region))

# The same will be repeated for the GDP Per Capital

un_per_capital <-read_csv("gdp.csv")

eu_per_capital <- un_per_capital |> 
  filter(region %in% eu_countries) |> 
  mutate(is_eu = "European Union") |> 
  group_by(year, is_eu) |> 
  summarize(per_capital = mean(gdp)) |> 
  rename("region" = is_eu) |> 
  relocate(region)

un_per_capital <- un_per_capital |> 
  rename("per_capital" = gdp) |> 
  bind_rows(eu_per_capital) |> 
  mutate(region = case_when(region == "United States" ~ "United States of America",
                            region == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                            region != "United States" ~ region))

un_data <- un_population |> 
  left_join(un_per_capital, join_by(region, year))

un_data <- un_data |> 
  group_by(region, year) |> 
  summarize(population = mean(population, na.rm = T),
            per_capital = mean(per_capital, na.rm = T))

un_data <- un_data[complete.cases(un_data), ]

ghg_data <- ghg_data |> 
  left_join(un_data, join_by(region, year))


ghg_data |> 
  filter(year == 2020) |> 
  ggplot(aes(population, per_capital, size = emission_value)) +
  geom_point(aes(col = region),
             show.legend = F) +
  theme_tinyhand()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                 labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
## Second plot for Dashboard, tab 2




ghg_data |> 
  left_join(un_population, join_by(region, year))


ghg_data <- ghg_data |> 
  group_by(gas, region) |> 
  mutate(cummulative_emission = cumsum(emission_value))

ghg_data <- ghg_data[complete.cases(ghg_data),]



ghg_data  |> 


######################################
ghg_data_long <- ghg_data |>
  group_by(series_code, country_or_area) |> 
  arrange(country_or_area, wt = year,
          .by_group = T) |>
  nest() |> 
  mutate(lagged_value = map(data, lag)) |> 
  unnest(cols = c(data, lagged_value),
         names_sep = "_") |> 
  select(c(1:4,6)) |> 
  rename("gas" = series_code,
         "region" = country_or_area,
         "year" = data_year,
         "value" = data_value,
         "lagged_value" = lagged_value_value) |> 
  mutate(percent_change = (value-lagged_value)/lagged_value * 100) |> 
  ungroup()

write.csv(ghg_data_long, "ghg_pivot_longer.csv")

ghg_data_long |> 
  filter(gas %in% c("N20", "CO2", "MIX", "CH4") & region %in% c("United States of America", "United Kingdom")) |> 
  filter(year >= 1999 & year <= 2019) |> 
  group_by(region, gas) |> 
  summarize(total = sum(value)) |> 
  pivot_wider(names_from = gas,
              values_from = total) |> 
  rowwise() |> 
  mutate(emistotal = sum(c_across(-1), na.rm = T))

na_to_zero <- function(x){
  ifelse(is.na(x), 0, x)
} # easily change all nas to 0

ghg_data_wide <- ghg_data_long |> 
  select(gas, region, year, value) |> 
  pivot_wider(names_from = gas,
              values_from = c(value),
              values_fn = mean) |>
  mutate(across(-c(1:2), na_to_zero))


ghg_data_wide_sample <- ghg_data_wide |> 
  left_join(un_population, join_by(region, year)) |> 
  relocate(region, year, population)


ggplotly(ghg_data_wide_sample |> 
           filter(year == 2010) |> 
           ggplot(aes(CH4, CO2, size = population))+
           geom_point(aes(col = region))+
           scale_x_log10() +
           scale_y_log10()+
           theme_minimal()+
           theme(legend.position = "none"))


ghg_data_long |>
  filter(region %in% c("Australia","Canada") & gas %in% c("CH4", "CO2")) |> 
  filter(year == 1999) |> 
  ggplot(aes(region, value, fill = gas))+
  geom_col()+
  theme_ipsum_es(grid = "Y")+
  scale_fill_viridis(discrete = T)+
  scale_y_continuous(label = scales::comma)

scales::
ghg_data_long

#####################################################################

na_to_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}

ghg_data_wide <- ghg_data_long |> 
  select(gas, region, year, value) |> 
  pivot_wider(names_from = gas,
              values_from = c(value),
              values_fn = mean) |>
  mutate(across(-c(1:2), na_to_zero))

# Define UI for application that draws a histogram
ui <-
  navbarPage("Climate Insights: Global Greenhouse Gas Emissions Tracker (1990-2020)",
             theme = shinytheme("cerulean"),
             
             # first tab
             tabPanel("Dashboard",
                      fluidPage(
                        # Page title
                        titlePanel("Interactive Emission Board"),
                        hr(),
                        selectInput("ghg", "Greenhouse gas",
                                    choices = unique(ghg_data_long$gas),
                                    multiple = T),
                        selectInput("country", "Select Region or Country",
                                    choices = unique(ghg_data_long$region),
                                    multiple = T),
                        tableOutput("total_emission"),
                        hr(),
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("years",
                                        "Year range:",
                                        min = min(ghg_data_long$year),
                                        max = max(ghg_data_long$year),
                                        value = c(1990, 2020)),
                            # Horizontal Line
                            checkboxInput("line_plot", "Line Plot", TRUE),
                            checkboxInput("area_plot", "Area Plot"),
                            checkboxInput("bar_plot", "Bar Plot"),
                          ),
                          # Print total
                          # Show a plot of the generated distribution
                          mainPanel(plotlyOutput("combined_plot"))
                        )
                      )
             ),
             
             # second tab
             tabPanel("Comparison Tool",
                      fluidPage(
                        titlePanel("Country and Gas Emission Comparison"),
                        
                        # Horizontal Line
                        hr(),
                        hr(),
                        sidebarPanel(
                          selectInput("ghg", "Greenhouse gas",
                                      choices = unique(ghg_data_long$gas),
                                      multiple = T),
                          selectInput("country", "Select Region or Country",
                                      choices = unique(ghg_data_long$region),
                                      multiple = T),
                          sliderInput("years",
                                      "Year range:",
                                      min = min(ghg_data_long$year),
                                      max = max(ghg_data_long$year),
                                      value = c(1990, 2020)),
                          checkboxInput("line_plot", "Line Plot", TRUE),
                          checkboxInput("bar_plot", "Bar Plot"),
                          
                          mainPanel(plotlyOutput("comparison_plot"))
                        )
                      )
             ),
             
             # third tab
             tabPanel("Predictive Modeling",
                      fluidPage(
                        titlePanel("Future Emission Forecast: Predictive Insights")
                        
                        #horizontal line
                        
                        # Horizontal Line
                      )
             ),
             
             # fourth tab
             tabPanel("Scenario Analysis"),
             
             # fifth tab
             tabPanel("Emissions Heatmap"),
             
             # sixth tab
             tabPanel("Correlation Analysis")
  )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # reactive element first tab
  plot_object <- reactive({
    req(input$ghg, input$country, input$years)
    
    ghg_data_long |> 
      filter(gas %in% input$ghg & region %in%input$country) |> 
      filter(year >= input$years[1] & year <= input$years[2])
  })
  
  # first tab table output
  output$total_emission <- renderTable({
    ghg_data_long |> 
      filter(gas %in% input$ghg & region %in% input$country) |> 
      filter(year >= input$years[1] & year <= input$years[2]) |> 
      group_by(region, gas) |>
      summarize(total = sum(value)) |> 
      pivot_wider(names_from = gas,
                  values_from = total)
  })
  
  
  # First tab plot
  output$combined_plot <- renderPlotly({
    emission_plot <- ggplot(plot_object(), aes(year, value)) +
      theme_ipsum_rc(grid = "XY") +
      scale_color_viridis(discrete = T) +
      scale_y_continuous(label = scales::comma) +
      labs(x = "year",
           y = "Emissions (KTCO2e)",
           title = paste0("Emission trend between", sep = " ", min(input$years), "and ", max(input$years))) +
      theme(legend.position = "bottom",
            axis.title = element_text(face = "bold"))
    
    if (input$line_plot) emission_plot <- emission_plot + geom_line(aes(col = region, lty = gas))
    if (input$area_plot) emission_plot <- emission_plot + geom_area(aes(fill = region), 
                                                                    position = "identity",alpha = 0.4)
    facet_wrap(~gas, scales = "free_y")
    if (input$bar_plot) emission_plot <- emission_plot + geom_col(aes(fill = gas)) +
      facet_wrap(. ~ region, scales = "free_y", ncol = 2)
    
    ggplotly(emission_plot)
    
  })
  
  # reactive element tab 2
  
  output$comparison_plot <- renderPlotly({
    comparison <- ggplot(plot_object())+
      theme_ipsum_es(grid = "XY")
    
    if(unique(length(input$years) >= 5) & input$line_plot) comparison <-
        comparison + geom_line(aes(years, value, col = region)) +
        scale_y_continuous(label = scales::comma)+
        labs(x = "year",
             y = "Emission (KTCO2e)",
             title = paste0("Emission from", sep = " ", min(input$years), sep = " ","to", max(input$years)))
    
    if(unique(length(input$years <= 5) & input$bar_plot)) comparison <- 
        comparison + geom_col(aes(region, value, fill = gas)) +
        scale_y_continuous(label = scales::comma)+
        labs(x = "Country",
             y = "Emission (KTCO2e)",
             title = paste0("Year", sep = " ", input$yearrs))
    
    ggplotly(comparison)
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)


#############################################################
# Population data needed for Comparison plot
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


un_population <- un_population |>
  select(`Country or Area`, Sex, Year, Area, Value) |> 
  filter(Sex == "Both Sexes" & Area == "Total") |> 
  select(c(1,3,5)) |> 
  rename("region" = "Country or Area",
         "year" = "Year",
         "population" = "Value")


EU <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
        "Estonia", "Finland", "France", "Germany", "Greece",
        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
        "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
        "Spain", "Sweden") # EU is not represented as a data point.
#list will be created to include EU in the data point


eu_pop <- un_population |> 
  filter(region %in% EU) |> 
  mutate(is_eu = "European Union") |> 
  group_by(year, is_eu) |> 
  summarize(population = sum(population)) |> 
  rename("region" = is_eu) |> 
  relocate(region)

un_population <- un_population |> 
  bind_rows(eu_pop)

# GDP data needed for comparison

# The same will be repeated for the GDP Per Capital

un_per_capital <-read_csv("gdp.csv")

eu_per_capital <- un_per_capital |> 
  filter(region %in% EU) |> 
  mutate(is_eu = "European Union") |> 
  group_by(year, is_eu) |> 
  summarize(per_capital = mean(gdp)) |> 
  rename("region" = is_eu) |> 
  relocate(region)

un_per_capital <- un_per_capital |> 
  rename("per_capital" = gdp) |> 
  bind_rows(eu_per_capital)

un_population |> 
  left_join(un_per_capital, join_by(region, year))
