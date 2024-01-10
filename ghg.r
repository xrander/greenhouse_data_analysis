# Load libraries
library("tidyverse")
library("plotly")
library("hrbrthemes")
library("janitor")
library("scales")
library("httr")
library("jsonlite")

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

ghg_data <- read_csv(content, col_types = cols()) %>% 
  clean_names()

gas_formula <- tibble(indicator = unique(ghg_data$indicator),
                      gas = c("CO2", "HFC", "CH4", "NO2", "PFC", "SF6", "GHG"))

ghg_data <- ghg_data %>% 
  left_join(gas_formula, join_by(indicator)) %>% 
  relocate(ref_area, gas) %>% 
  left_join(countrycode::codelist %>% select(country.name.en, genc3c),
            join_by("ref_area" == "genc3c")) %>% 
  relocate(country.name.en) %>% 
  rename("region" = country.name.en,
         "year" = time_period,
         "emission_value" = obs_value) %>% 
  select(c(1, 3, 8, 9)) %>% 
  mutate(region = ifelse(is.na(region), "European Union", region),
         year = as.integer(year)) %>%
  mutate_if(is_character, factor)


ghg_data <- ghg_data %>% 
  mutate(region = case_when(region == "United States" ~ "United States of America",
                             region != "United States" ~ region))

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
} # output needed to be in readable format and not long numbers


# Metrics needed for bubble chart
un_population <- read_csv("https://raw.githubusercontent.com/xrander/greenhouse_data_analysis/master/data/population_data.csv",
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
         "population" = "Value") %>% 
  mutate(year = as.integer(year),
         region = case_when(region == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                            region != "United Kingdom of Great Britain and Northern Ireland" ~ region ))


european_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
                        "Estonia", "Finland", "France", "Germany", "Greece",
                        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
                        "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
                        "Spain", "Sweden") # EU is not represented as a data point.
#list will be created to include EU in the data point

eu_pop <- un_population %>% 
  filter(region %in% european_countries) %>% 
  mutate(is_eu = "European Union") %>% 
  group_by(year, is_eu) %>% 
  summarize(population = sum(population)) %>% 
  rename("region" = is_eu) %>% 
  relocate(region)

un_population <- un_population %>% 
  bind_rows(eu_pop)

# The same will be repeated for the GDP Per Capital

un_per_capital <-read_csv("https://raw.githubusercontent.com/xrander/greenhouse_data_analysis/master/data/gdp.csv") %>% 
  mutate(region = case_when(region == "United States" ~ "United States of America",
                            region == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                            region != "United States" ~ region))

eu_per_capital <- un_per_capital %>% 
  filter(region %in% european_countries) %>% 
  mutate(is_eu = "European Union") %>% 
  group_by(year, is_eu) %>% 
  summarize(per_capital = mean(gdp)) %>% 
  rename("region" = is_eu) %>% 
  relocate(region)


un_per_capital <- un_per_capital %>% 
  rename("per_capital" = "gdp") %>% 
  bind_rows(eu_per_capital)

un_data <- un_population %>% 
  left_join(un_per_capital, join_by(region, year))

# Count missing data
un_data %>% 
  filter(is.na(per_capital)) %>% 
  count(region) %>% 
  filter(region %in% ghg_data$region) %>% 
  ggplot(aes(fct_reorder(region, n), n))+
  geom_col()+
  coord_flip()

un_data <- un_data %>% 
  group_by(region, year) %>% 
  summarize(population = mean(population, na.rm = T),
            per_capital = mean(per_capital, na.rm = T))

un_data <- un_data[complete.cases(un_data), ]


ghg_data <- ghg_data %>% 
  left_join(un_data, join_by(region, year))

ghg_data %>% 
  filter(is.na(population) & is.na(per_capital)) %>% 
  count(region) %>% 
  ggplot(aes(region, n, fill = region))+
  geom_col()+
  coord_flip()

ghg_data <- ghg_data %>% 
  group_by(gas, region) %>% 
  mutate(cummulative_emission = cumsum(emission_value),
         year = as.integer(year)) %>% 
  ungroup()
  
ghg_data %>% 
  ggplot(aes(y = fct_reorder(region, emission_value), x = emission_value, fill = gas)) +
  ggridges::geom_density_ridges(alpha = 0.6, bandwidth = 1000000) +
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  theme_tinyhand() # template for comp tab

ghg_data %>% 
  filter(gas %in% c("CO2", "CH4", "GHG") & year %in% c(2010:2015)) %>% 
  ggplot(aes(x = fct_reorder(factor(year), emission_value), y = emission_value, fill = gas))+
  geom_violin() +
  scale_fill_viridis_d()+
  scale_y_log10() # template two for comp tab


##########################################################
# Map creation for tab 2 : comparison plot
# Bins needed for fill aesthetic to create chloropeth

# Uncomment code below to see the varying max value for each gas

# ghg_data %>% 
#   group_by(gas) %>% 
#   summarize(max_emission = max(emission_value),
#             min_emission = min(emission_value))


# Bins for larger value group like CO2 and GHG
my_bin <- function(x) {
  cut(x,
      breaks =c(0, 1000000, 2500000, 3500000, 5000000, 10000000),
      labels = c("0 - 1,000,000",
                 "1,000,000 - 2,500,000",
                 "2,500,000 - 3,500,000",
                 "3,500,000 - 5,000,000",
                 "5,000,000 - 10,000,000"))
}

# Bins for med value group
small_bin <- function(x) {
  cut(x,
      breaks = c(0, 50000, 100000, 250000, 500000, 1000000),
      labels = c("0 - 50000",
                 "50000 - 100000",
                 "100000 - 250000",
                 "250000 - 500000",
                 "500000 - 1000000"))
}

# Bins for tiny value group
tiny_bin <- function(x) {
  cut(x,
      breaks = c(0, 5000, 10000, 25000, 50000, 250000),
      labels = c("0 - 5000",
                 "5000 - 10000",
                 "10000 - 25000",
                 "25000 - 50000",
                 "50000 - 250000"))
}

ghg_map <- ghg_data %>% 
  select(region:per_capital) %>%
  mutate(emission_bin = cut(emission_value,
                            breaks =c(0, 1000000, 2500000, 3500000, 5000000, 10000000),
                            labels = c("0 - 1,000,000",
                                       "1,000,000 - 2,500,000",
                                       "2,500,000 - 3,500,000",
                                       "3,500,000 - 5,000,000",
                                       "5,000,000 - 10,000,000"))) %>% 
  group_by(gas, emission_bin) %>% 
  summarize(n())
  pivot_wider(names_from = gas,
              values_from = emission_value) %>% 
  mutate(region = case_when(region == "Czechia" ~ "Czech Republic",
                            region != "Czechia" ~ region))


#####################################################################

ghg_data %>% 
  group_by(region, year) %>% 
  summarize(emission_value = sum(emission_value),
            cummulative_emission = sum(cummulative_emission),
            population = mean(population),
            per_Capital = mean(per_capital)) %>% 
  ggplot(aes(fct_reorder(region, emission_value), emission_value, fill = emission_value)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_c(direction = -1) +
  coord_flip() +
  transition_time(as.integer(year)) +
  labs(title = "Year: {frame_time}",
       x = "Region",
       y = "Emission in GgCO2e" ) +
  view_follow() +
  ease_aes("cubic-in")



####

ghg_data %>% 
  filter(year == 2020) %>% 
  ggplot(aes(population, per_capital, size = emission_value)) +
  geom_point(aes(col = region),
             show.legend = F) +
  theme_tinyhand()+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
## Second plot for Dashboard, tab 2




ghg_data %>% 
  left_join(un_population, join_by(region, year))


ghg_data <- ghg_data %>% 
  group_by(gas, region) %>% 
  mutate(cummulative_emission = cumsum(emission_value))

ghg_data <- ghg_data[complete.cases(ghg_data),]



ghg_data %>% 
  group_by(region, year) %>% 
  summarize(emission_value = sum(emission_value)) %>% 
  arrange(desc(emission_value)) %>% 
  filter(emission_value >= 200000) %>% 
  pull(region) %>% 
  unique() %>% 
  sort()


ghg_data %>% 
  mutate(region = fct_collapse(region,
                               "Others" = c(#"Turkey", "Netherlands","Romania", 
                                 "Belarus", "Greece",#"Czechia", "Belgium",
                                 "Bulgaria", "Hungary", "Denmark",
                                 "Austria",  "Portugal", "Finland",
                                 "New Zealand", "Sweden", "Slovakia", 
                                 "Ireland", "Norway", "Switzerland", 
                                 "Lithuania", "Estonia", "Croatia",
                                 "Latvia", "Slovenia", "Luxembourg",
                                 "Cyprus", "Iceland", "Malta", "Liechtenstein", "Monaco")
  )
  )
#####################################################################

na_to_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}

ghg_data_wide <- ghg_data_long %>% 
  select(gas, region, year, value) %>% 
  pivot_wider(names_from = gas,
              values_from = c(value),
              values_fn = mean) %>%
  mutate(across(-c(1:2), na_to_zero))

# Define UI for application that draws a histogram
# ui <-
#   navbarPage("Climate Insights: Global Greenhouse Gas Emissions Tracker (1990-2020)",
#              theme = shinytheme("cerulean"),
#              
#              # first tab
#              tabPanel("Dashboard",
#                       fluidPage(
#                         # Page title
#                         titlePanel("Interactive Emission Board"),
#                         hr(),
#                         selectInput("ghg", "Greenhouse gas",
#                                     choices = unique(ghg_data_long$gas),
#                                     multiple = T),
#                         selectInput("country", "Select Region or Country",
#                                     choices = unique(ghg_data_long$region),
#                                     multiple = T),
#                         tableOutput("total_emission"),
#                         hr(),
#                         # Sidebar with a slider input for number of bins 
#                         sidebarLayout(
#                           sidebarPanel(
#                             sliderInput("years",
#                                         "Year range:",
#                                         min = min(ghg_data_long$year),
#                                         max = max(ghg_data_long$year),
#                                         value = c(1990, 2020)),
#                             # Horizontal Line
#                             checkboxInput("line_plot", "Line Plot", TRUE),
#                             checkboxInput("area_plot", "Area Plot"),
#                             checkboxInput("bar_plot", "Bar Plot"),
#                           ),
#                           # Print total
#                           # Show a plot of the generated distribution
#                           mainPanel(plotlyOutput("combined_plot"))
#                         )
#                       )
#              ),
#              
#              # second tab
#              tabPanel("Comparison Tool",
#                       fluidPage(
#                         titlePanel("Country and Gas Emission Comparison"),
#                         
#                         # Horizontal Line
#                         hr(),
#                         hr(),
#                         sidebarPanel(
#                           selectInput("ghg", "Greenhouse gas",
#                                       choices = unique(ghg_data_long$gas),
#                                       multiple = T),
#                           selectInput("country", "Select Region or Country",
#                                       choices = unique(ghg_data_long$region),
#                                       multiple = T),
#                           sliderInput("years",
#                                       "Year range:",
#                                       min = min(ghg_data_long$year),
#                                       max = max(ghg_data_long$year),
#                                       value = c(1990, 2020)),
#                           checkboxInput("line_plot", "Line Plot", TRUE),
#                           checkboxInput("bar_plot", "Bar Plot"),
#                           
#                           mainPanel(plotlyOutput("comparison_plot"))
#                         )
#                       )
#              ),
#              
#              # third tab
#              tabPanel("Predictive Modeling",
#                       fluidPage(
#                         titlePanel("Future Emission Forecast: Predictive Insights")
#                         
#                         #horizontal line
#                         
#                         # Horizontal Line
#                       )
#              ),
#              
#              # fourth tab
#              tabPanel("Scenario Analysis"),
#              
#              # fifth tab
#              tabPanel("Emissions Heatmap"),
#              
#              # sixth tab
#              tabPanel("Correlation Analysis")
#   )
# 
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#   
#   # reactive element first tab
#   plot_object <- reactive({
#     req(input$ghg, input$country, input$years)
#     
#     ghg_data_long %>% 
#       filter(gas %in% input$ghg & region %in%input$country) %>% 
#       filter(year >= input$years[1] & year <= input$years[2])
#   })
#   
#   # first tab table output
#   output$total_emission <- renderTable({
#     ghg_data_long %>% 
#       filter(gas %in% input$ghg & region %in% input$country) %>% 
#       filter(year >= input$years[1] & year <= input$years[2]) %>% 
#       group_by(region, gas) %>%
#       summarize(total = sum(value)) %>% 
#       pivot_wider(names_from = gas,
#                   values_from = total)
#   })
#   
#   
#   # First tab plot
#   output$combined_plot <- renderPlotly({
#     emission_plot <- ggplot(plot_object(), aes(year, value)) +
#       theme_ipsum_rc(grid = "XY") +
#       scale_color_viridis(discrete = T) +
#       scale_y_continuous(label = scales::comma) +
#       labs(x = "year",
#            y = "Emissions (KTCO2e)",
#            title = paste0("Emission trend between", sep = " ", min(input$years), "and ", max(input$years))) +
#       theme(legend.position = "bottom",
#             axis.title = element_text(face = "bold"))
#     
#     if (input$line_plot) emission_plot <- emission_plot + geom_line(aes(col = region, lty = gas))
#     if (input$area_plot) emission_plot <- emission_plot + geom_area(aes(fill = region), 
#                                                                     position = "identity",alpha = 0.4)
#     facet_wrap(~gas, scales = "free_y")
#     if (input$bar_plot) emission_plot <- emission_plot + geom_col(aes(fill = gas)) +
#       facet_wrap(. ~ region, scales = "free_y", ncol = 2)
#     
#     ggplotly(emission_plot)
#     
#   })
#   
#   # reactive element tab 2
#   
#   output$comparison_plot <- renderPlotly({
#     comparison <- ggplot(plot_object())+
#       theme_ipsum_es(grid = "XY")
#     
#     if(unique(length(input$years) >= 5) & input$line_plot) comparison <-
#         comparison + geom_line(aes(years, value, col = region)) +
#         scale_y_continuous(label = scales::comma)+
#         labs(x = "year",
#              y = "Emission (KTCO2e)",
#              title = paste0("Emission from", sep = " ", min(input$years), sep = " ","to", max(input$years)))
#     
#     if(unique(length(input$years <= 5) & input$bar_plot)) comparison <- 
#         comparison + geom_col(aes(region, value, fill = gas)) +
#         scale_y_continuous(label = scales::comma)+
#         labs(x = "Country",
#              y = "Emission (KTCO2e)",
#              title = paste0("Year", sep = " ", input$yearrs))
#     
#     ggplotly(comparison)
#   })
#   
# }
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
# 
# #############################################################
# # Population data needed for Comparison plot
# un_population <- read_csv("population_data.csv",
#                           col_types = list("Country or Area" = col_character(),
#                                            "Year" = col_double(),
#                                            "Area" = col_character(),
#                                            "Sex" = col_character(),
#                                            "Record Type" = col_character(),
#                                            "Value" = col_double(),
#                                            "Value Footnotes" = col_double()
#                           )
# )
# 
# 
# un_population <- un_population %>%
#   select(`Country or Area`, Sex, Year, Area, Value) %>% 
#   filter(Sex == "Both Sexes" & Area == "Total") %>% 
#   select(c(1,3,5)) %>% 
#   rename("region" = "Country or Area",
#          "year" = "Year",
#          "population" = "Value")
# 
# 
# EU <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", 
#         "Estonia", "Finland", "France", "Germany", "Greece",
#         "Hungary", "Ireland", "Italy", "Latvia", "Lithuania",
#         "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia",
#         "Spain", "Sweden") # EU is not represented as a data point.
# #list will be created to include EU in the data point
# 
# 
# eu_pop <- un_population %>% 
#   filter(region %in% EU) %>% 
#   mutate(is_eu = "European Union") %>% 
#   group_by(year, is_eu) %>% 
#   summarize(population = sum(population)) %>% 
#   rename("region" = is_eu) %>% 
#   relocate(region)
# 
# un_population <- un_population %>% 
#   bind_rows(eu_pop)
# 
# # GDP data needed for comparison
# 
# # The same will be repeated for the GDP Per Capital
# 
# un_per_capital <-read_csv("gdp.csv")
# 
# eu_per_capital <- un_per_capital %>% 
#   filter(region %in% EU) %>% 
#   mutate(is_eu = "European Union") %>% 
#   group_by(year, is_eu) %>% 
#   summarize(per_capital = mean(gdp)) %>% 
#   rename("region" = is_eu) %>% 
#   relocate(region)
# 
# un_per_capital <- un_per_capital %>% 
#   rename("per_capital" = gdp) %>% 
#   bind_rows(eu_per_capital)
# 
# un_population %>% 
#   left_join(un_per_capital, join_by(region, year))
