setwd("~/Documents/Data Science/Personal Project/ghg_data_analysis/")

library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("tidyverse")
library("ggridges")
library("plotly")
library("hrbrthemes")
library("scales")
library("janitor")
library("httr")
library("jsonlite")


base_url <- "https://data.un.org/ws/rest/data/UNSD,DF_UNData_UNFCC,1.0/.EN_ATM_METH_XLULUCF+EN_ATM_CO2E_XLULUCF+EN_CLC_GHGE_XLULUCF+EN_ATM_HFCE+EN_ATM_NO2E_XLULUCF+EN_ATM_PFCE+EN_ATM_SF6E.AUS+AUT+BLR+BEL+BGR+CAN+HRV+CYP+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ITA+JPN+LVA+LIE+LTU+LUX+MLT+MCO+NLD+NZL+NOR+POL+PRT+ROU+RUS+SVK+SVN+ESP+SWE+CHE+TUR+UKR+GBR+USA+EU1./ALL/?detail=full&dimensionAtObservation=TIME_PERIOD"
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
         "population" = "Value")


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
  bind_rows(eu_pop) %>% 
  mutate(region = ifelse(region == "United Kingdom of Great Britain and Northern Ireland",
                         "United Kingdom", region))

# The same will be repeated for the GDP Per Capital

un_per_capital <-read_csv("https://raw.githubusercontent.com/xrander/greenhouse_data_analysis/master/data/gdp.csv")

eu_per_capital <- un_per_capital %>% 
  filter(region %in% european_countries) %>% 
  mutate(is_eu = "European Union") %>% 
  group_by(year, is_eu) %>% 
  summarize(per_capital = mean(gdp)) %>% 
  rename("region" = is_eu) %>% 
  relocate(region)

un_per_capital <- un_per_capital %>% 
  rename("per_capital" = gdp) %>% 
  bind_rows(eu_per_capital) %>% 
  mutate(region = case_when(region == "United States" ~ "United States of America",
                            region == "United Kingdom of Great Britain and Northern Ireland" ~ "United Kingdom",
                            region != "United States" ~ region))

un_data <- un_population %>% 
  left_join(un_per_capital, join_by(region, year))

un_data <- un_data %>% 
  group_by(region, year) %>% 
  summarize(population = mean(population, na.rm = T),
            per_capital = mean(per_capital, na.rm = T))

un_data <- un_data[complete.cases(un_data), ]

ghg_data <- ghg_data %>% 
  left_join(un_data, join_by(region, year))


ghg_data <- ghg_data %>% 
  group_by(gas, region) %>% 
  mutate(cummulative_emission = cumsum(emission_value),
         year = as.integer(year)) %>% 
  ungroup()

ui <- ui <- dashboardPage(skin = "green",
                          dashboardHeader(title = "GHG Emission Explorer (1990 - 2020)",
                                          titleWidth = 400),
                          dashboardSidebar(
                            sidebarMenu(
                              menuItem("Overview", tabName = "overview",
                                       icon = icon("dashboard", lib = "glyphicon")),
                              menuItem("Comparison Tool", tabName = "comparison",
                                       icon = icon("duplicate", lib = "glyphicon")), # to include map
                              menuItem("Scenario Analysis", tabName = "scenario",
                                       icon = icon("hourglass", lib = "glyphicon")),
                              menuItem("Predictive Analysis", tabName = "predictive_analysis",
                                       icon = icon("stats", lib = "glyphicon"))
                              )
                            ),
                          dashboardBody(
                            tabItems(
                              tabItem(
                                tabName = "overview",
                                h1("Overview of Emissions"),
                                
                                fluidRow(
                                  box(
                                    pickerInput("single_year", "Select Year",
                                                choices = unique(ghg_data$year),
                                                options = list(`actions-box` = T),
                                                multiple = T,
                                                selected = 2010),
                                    pickerInput("single_gas", "Select Greenhouse Gas",
                                                choices = unique(ghg_data$gas),
                                                options = list(`actions-box` = T),
                                                multiple = T,
                                                selected = "CH4"),
                                    width = 2,
                                    background = "olive"
                                  ),
                                  
                                  infoBoxOutput("total_emission", width = 2),
                                  
                                  box(
                                    title = "Top Emitting Countries",
                                    width = 4,
                                    status = "danger",
                                    solidHeader = T,
                                    collapsible = T,
                                    plotOutput("top_emitting_countries")
                                  ), 
                                
                                  box(
                                    width = 4,
                                    title = "Proportion of Gas Emission",
                                    plotlyOutput("piechart"),
                                    status = "warning",
                                    solidHeader = T,
                                    collapsible = T
                                    )
                                  ),
                                
                                fluidRow(
                                  column(width = 12,
                                         box(
                                           width = 3,
                                           background = "green",
                                           sliderTextInput("year_range", "Choose year range",
                                                           choices = sort(unique(ghg_data$year)),
                                                           selected = c(1990, 2000),
                                                           from_min = 1990,
                                                           from_max = 2005,
                                                           to_min = 2010,
                                                           to_max = 2020)
                                           ),
                                         box(
                                           status = "warning",
                                           solidHeader = T,
                                           plotlyOutput("line_plot"),
                                           width = 9
                                           )
                                         )
                                  )
                                
                                
                                ),

                            tabItem(
                              tabName = "comparison",
                              h2("Comparison Tool"),
                              
                              fluidRow(
                                box(
                                  h6("Plot on the right shows how gasas compares across overtime.
                                     Select a gas to begin 'Methane(CH4)' is chosen by default, click dropdown to make choice"),
                              
                                  hr(),
                                  pickerInput("region", " Countries: ",
                                              choices = unique(ghg_data$region),
                                              multiple = T,
                                              selected = c("United States", "United Kingdom")),
                                  pickerInput("gas_select", "Gases:",
                                              choices = unique(ghg_data$gas),
                                              multiple = T,
                                              selected = c("CH4", "CO2")),
                                  width = 3),
                                box(
                                  title = "Regional Comparison of Gas Emission",
                                  width = 9,
                                  plotOutput("comp_ridges"),
                                  collapsible = T),
                                box()
                              ),
                              
                              fluidRow(
                                box(
                                 pickerInput("single_year_2", "Year: ",
                                             choices = sort(unique(ghg_data$year))),
                                 hr(),
                                 pickerInput("region2", " Countries: ",
                                             choices = unique(ghg_data$region),
                                             multiple = T,
                                             selected = c("United States", "United Kingdom")),
                                 width = 3),
                                
                                box(plotlyOutput("comp_bar_plot"),
                                    width = 9)
                                )
                                
                              ),
                            
                            tabItem(
                              tabName = "scenario",
                              h2("Scenario Analysis")
                              ),
                            
                            
                            tabItem(
                              tabName = "predictive_analysis",
                              h2("Predictive Analysis")
                              )
                            )
                            )
                          )



server <- function(input, output) {
  
  output$total_emission <- renderInfoBox({
     infoBox(
       "Total Emission",
       paste0(ghg_data %>% 
                filter(year %in% input$single_year & gas %in% input$single_gas) %>% 
                group_by(gas) %>% 
                summarize(total_emission = sum(emission_value)) %>% 
                pull(total_emission) %>% 
                sum() %>% 
                format_large_number()," KTCO2e", sep = " "),
       icon = icon("cloud", lib = "glyphicon"), color = "red",
       fill = T)
    })
  
  output$top_emitting_countries <- renderPlot({
    ghg_data %>% 
      filter(year %in% input$single_year & gas %in% input$single_gas) %>% 
      group_by(region) %>% 
      summarize(total_emission = sum(emission_value)) %>% 
      arrange(desc(total_emission)) %>% 
      top_n(n = 10, wt = total_emission) %>% 
      ggplot(aes(fct_reorder(region, total_emission), total_emission, fill = total_emission)) +
      geom_bar(stat = "identity",
               show.legend = F) +
      labs(x = "Region",
           y = "Emission in KTCO2e") +
      scale_fill_distiller(palette = "Reds",
                           direction = 1) +
      scale_y_continuous(label = scales::comma) +
      coord_flip() +
      theme_tinyhand() +
      theme(axis.title = element_text(face = "bold",
                                      size = 12),
            axis.text = element_text(face = "bold.italic",
                                     size = 9))
  })
  
  output$piechart <- renderPlotly({
    ghg_data %>%
      filter(year %in% input$single_year) %>% 
      group_by(gas) %>%
      summarize(total_emission = sum(emission_value)) %>%
      mutate(gas_proportion = round(total_emission/sum(total_emission) * 100, 1),
             ylab_pos = cumsum(gas_proportion + 1) - 0.5 * gas_proportion) %>% 
      plot_ly(labels = ~gas, values = ~gas_proportion, type = "pie",
              textposition = "inside",
              textinfo = "label+percent",
              showlegend = F)

  })
  
  output$line_plot <- renderPlotly({
    emis_plot <- ghg_data %>% 
      filter(between(year, min(input$year_range), max(input$year_range))) %>%
      group_by(year, gas) %>% 
      summarize(emission_value = sum(emission_value)) %>% 
      ggplot(aes(year, emission_value, col = gas, fill = gas)) +
      geom_line()+
      geom_area(alpha = 0.7)+
      theme_tinyhand() +
      labs(y = "Emissions (KTCO2e)",
           x = "Year",
           title = paste0("Emission from", sep = "", min(input$year_range), " to ", max(input$year_range))) +
      
      scale_y_continuous(labels = scales::comma)# 4th result to be used for dashboard 1
    
    ggplotly(emis_plot)
  })
  
  output$comp_ridges <- renderPlot({
    ghg_data %>% 
      filter(region %in% input$region & gas %in% input$gas_select) %>% 
      ggplot(aes(y = fct_reorder(region, emission_value), x = emission_value, fill = gas)) +
      geom_density_ridges(alpha = 0.5, bandwidth = 1000000) +
      scale_fill_viridis_d() +
      labs(col = "",
           x = "Emission in GgCO2e",
           y = "Region") +
      theme_tinyhand()+
      scale_x_comma()
    
  })
  
  output$comp_bar_plot <- renderPlotly({
    ghg_data %>% 
      filter(year == input$single_year_2 & region %in% input$region2) %>% 
      ggplot(aes(gas, cummulative_emission, fill = region)) +
      geom_bar(stat = "identity",
               position = "fill") +
      scale_fill_viridis_d()+
      labs(title = paste0("Emission Proportion in Year", input$single_year_2),
           x = "Greenhouse Gases",
           y = "Proportion of Emission")
  })
  
  
  
  output$animated_bar_plot <- renderPlot({
    ghg_data %>% 
      filter(gas == input$gas2) %>% 
      if (input$cumsum == TRUE) {
        return(
          ggplot(aes(fct_reorder(region, cummulative_emission),
                     cummulative_emission,
                     fill = emission_value)) +
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
      )}
    
      else {
        return(
          ggplot(aes(fct_reorder(region, emission_value),
                     emission_value,
                     fill = emission_value)) +
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
          )}
      
  })
  
  }

shinyApp(ui, server)
