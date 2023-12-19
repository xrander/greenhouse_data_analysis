library("shiny")
library("shinythemes")
library("tidyverse")
library("janitor")

ghg_data_long <- read_csv("ghg_pivot_longer.csv")

ghg_data_long <- ghg_data_long %>% 
  select(2:7)

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
                          mainPanel(plotOutput("combined_plot"))
                          )
                        )
                      ),
             
             # second tab
             tabPanel("Comparison Tool",
                      fluidPage(
                        titlePanel("Country and Gas Emission Comparison")
                        )
                      ),
             
             # third tab
             tabPanel("Predictive Modeling",
                      fluidPage(
                        titlePanel("Future Emission Forecast: Predictive Insights")
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
  output$total_emission <- renderTable({
    ghg_data_long %>% 
      filter(gas %in% input$ghg & region %in% input$country) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      group_by(region, gas) %>%
      summarize(total = sum(value)) %>% 
      pivot_wider(names_from = gas,
                  values_from = total)
  })
  
  plot_object <- reactive({
    req(input$ghg, input$country, input$years)
    
    ghg_data_long %>% 
      filter(gas %in% input$ghg & region %in%input$country) %>% 
      filter(year >= input$years[1] & year <= input$years[2])
  })
  

  output$combined_plot <- renderPlot({
   emission_plot <- ggplot(plot_object(), aes(year, value)) +
     theme_minimal() +
     labs(x = "year",
          y = "Emission in Kilotonne")+
     theme(legend.position = "top")
   if (input$line_plot) emission_plot <- emission_plot + geom_line(aes(col = region, lty = gas))
   if (input$area_plot) emission_plot <- emission_plot + geom_area(aes(fill = region) ,alpha = 0.6) +
       facet_wrap(~gas, scales = "free_y")
   if (input$bar_plot) emission_plot <- emission_plot + geom_col(aes(fill = region), position = "dodge") +
       facet_wrap(~gas, scales = "free_y")
   
   emission_plot
  }, res = 96)
  }
# Run the application 
shinyApp(ui = ui, server = server)
