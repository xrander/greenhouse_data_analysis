library("shiny")
library("shinythemes")
library("tidyverse")
library("janitor")

ghg_data_long <- read_csv("ghg_pivot_longer.csv")

ghg_data_long <- ghg_data_long %>% 
  select(2:7)

# Define UI for application that draws a histogram
ui <-
  navbarPage("Climate Insights: Global Greenhouse Gas Emissions Tracker (1990-2020)",
             theme = shinytheme("cerulean"),
             
             # first tab
             tabPanel("Dashboard",
                      fluidPage(
                        # Application title
                        titlePanel("Interactive Emission Board"),
                        hr(),
                        selectInput("ghg", "Greenhouse gas",
                                    choices = unique(ghg_data_long$gas),
                                    multiple = T),
                        selectInput("country", "Select Region or Country",
                                    choices = unique(ghg_data_long$region),
                                    multiple = T),
                        tableOutput("total_emission"),
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("years",
                                        "Year range:",
                                        min = min(ghg_data_long$year),
                                        max = max(ghg_data_long$year),
                                        value = c(1990, 2020)),
                            checkboxGroupInput("plot_type", "Select Plot Type",
                                               choices = c("line", "area", "bar"),
                                               selected = "line")
                            ),
                          # Print total
                          # Show a plot of the generated distribution
                          mainPanel(
                            plotOutput("line_plot"),
                            plotOutput("area_plot"),                   
                            plotOutput("bar_plot")
                            )
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
                  values_from = total) %>% 
      rowwise() %>% 
      mutate(emission_total = sum(c_across(-1), na.rm = T))
  })
  
  plot_object <- reactive({
    req(input$ghg, input$country, input$years)
    
    ghg_data_long %>% 
      filter(gas == input$ghg & region == input$country) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      ggplot(aes(year, value))
  })
  

  output$line_plot <- renderPlot({
   plot_object() +
      geom_line(aes(col = region, lty = gas))+
      labs(y = "Emission (kilitone)",
           x = "year")+
      theme_minimal()+
      theme(legend.position = "right")
     }, res = 96)
  
  output$area_plot <- renderPlot({
    plot_object() +
      geom_area(aes(fill = region), alpha = 0.6) +
      labs(y = "Emission (kilitone)",
           x = "year") +
      theme_minimal() +
      theme(legend.position = "top")+
      facet_wrap(~gas, scales = "free_y")
    }, res = 96)
  
  output$bar_plot <- renderPlot({
    plot_object() +
      geom_col(aes(fill = region), position = "dodge") +
      labs(y = "Emission (kilitone)",
           x = "year") +
      theme_minimal() +
      theme(legend.position = "top")+
      facet_wrap(~gas, scales = "free_y")
      }, res = 96)
}
# Run the application 
shinyApp(ui = ui, server = server)
