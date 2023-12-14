library("shiny")
library("shinythemes")
library("tidyverse")
library("janitor")


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
  ),
  # Application title
  titlePanel("Global Emissions Explorer: Interactive Analysis of Greenhouse Gas Trends (1990-2020)"),
  selectInput("ghg", "Greenhouse gas", choices = unique(ghg_data_long$gas)),
  selectInput("country", "Greenhouse gas", choices = unique(ghg_data_long$region), multiple = T),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("years",
                  "Year range:",
                  min = min(ghg_data_long$year),
                  max = max(ghg_data_long$year),
                  value = c(1990, 2020))
      ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("displot"),
      plotOutput("displot2"),
      plotOutput("displot3")
    )
  )
)  

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$displot <- renderPlot({
    ghg_data_long %>% 
      filter(gas == input$ghg & region == input$country) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      ggplot(aes(year, value, color = region))+
      geom_line(show.legend = NA)+
      labs(y = "Emission (kilitone)",
           x = "year")+
      theme_minimal()+
      theme(legend.position = "none")
     })
  
  output$displot2 <- renderPlot({
    ghg_data_long %>% 
      filter(gas == input$ghg & region == input$country) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      ggplot(aes(year, value, fill = region))+
      geom_area(alpha = 0.6)+
      labs(y = "Emission (kilitone)",
           x = "year")+
      theme_minimal()+
      theme(legend.position = "none")
    })
  
  output$displot3 <- renderPlot({
    ghg_data_long %>% 
      filter(gas == input$ghg & region == input$country) %>% 
      filter(year >= input$years[1] & year <= input$years[2]) %>% 
      ggplot(aes(year, value, fill = region))+
      geom_col(position = "dodge")+
      labs(y = "Emission (kilitone)",
           x = "year")+
      theme_minimal()+
      theme(legend.position = "none")
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
