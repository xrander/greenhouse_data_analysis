# Set work directory
setwd("~/Documents/Data Science/Personal Project/ghg_data_analysis/")

# 
library("tidyverse")
library("plotly")
library("hrbrthemes")
library("viridis")
library("janitor")

files <- list.files(pattern = "\\.csv$", full.names = T)

# files
files <- files[-c(1,2)] # previously saved csv files from when script was original developed are removed.
# uncomment and run 'files' first before running this code.


ghg_data <- map_df(files, read_csv) %>% 
  clean_names() %>% 
  select(-country_or_area_code) %>% 
  rename("gas" = series_code,
         "region" = country_or_area,
         "emission_value" = value) %>% 
  mutate_if(is.character, factor)


skimr::skim_without_charts(ghg_data)

ghg_data %>% 
  group_by(region, gas) %>% 
  count(gas)

ghg_data %>%
  group_by(gas) %>% 
  summarize(number = n()) %>% 
  ggplot(aes(fct_reorder(gas, number), number, fill = gas))+
  geom_col()+
  theme_ipsum_es()+
  coord_flip()

ghg_data %>% 
  group_by(year, gas) %>% 
  summarize(total_emissions = sum(emission_value)) %>% 
  pull(total_emissions) %>% 
  sum() %>%
  round(1) %>% 
  paste0(" KCO2e", sep = " ")# First table result for dashboard 1
  
ghg_data %>%
  # to be filtered by year (year %in% year) 
  mutate(gas = fct_collapse(gas,
                            "Others" = c("HFC", "MIX", "N2O", "NF3", "PFC", "SF6"))) %>% 
  group_by(gas) %>%
  summarize(emission = sum(emission_value)) %>%
  mutate(gas_proportion = round(emission/sum(emission) * 100, 1),
         ylab_pos = cumsum(gas_proportion) - 0.5 * gas_proportion) %>% 
  ggplot(aes(x = 1, gas_proportion, fill = gas))+
  geom_col(position = "stack", width = 1, color = "white")+
  coord_polar("y", direction = 1, start = 0)+
  labs(fill = "")+
  geom_text(aes(x = 1,
                y = ylab_pos,
                label = paste0(gas_proportion, "%", sep = "")),
            col = "white")  +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 24, color = "gray"))+
  theme_void()

#  second plot, dashboard 1 result,
# should only filter by year as some of the gases emission are too low and disrupts the visualization,
# they are therefore collapsed.

ghg_data %>% 
  # to be filtered by year and gas
  group_by(region) %>% 
  summarize(total_emission = sum(emission_value)) %>% 
  arrange(desc(total_emission)) %>% 
  top_n(n = 10, wt = total_emission) # result 3 to be used for dashboard 1, title: top_emitting_countries. 
  

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


ghg_data %>% 
  filter(region == "United States of America", between(year, 1990, 2020)) %>%  # region and year are input for shiny
  ggplot(aes(year, emission_value, col = gas))+
  geom_line()+
  #geom_area(aes(fill = gas), alpha = 0.7)+
  #scale_y_log10()+
  scale_y_continuous(labels = scales::comma)# 4th result to be used for dashboard 1

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

scales::
ghg_data_long

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
    
    ghg_data_long %>% 
      filter(gas %in% input$ghg & region %in%input$country) %>% 
      filter(year >= input$years[1] & year <= input$years[2])
  })
  
  # first tab table output
  output$total_emission <- renderTable({
    ghg_data_long %>% 
      filter(gas %in% input$ghg & region %in% input$country) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      group_by(region, gas) %>%
      summarize(total = sum(value)) %>% 
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
