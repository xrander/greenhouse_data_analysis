library("shiny")
library("shinydashboard")
library("shinyWidgets")
library("tidyverse")
library("plotly")
library("hrbrthemes")
library("viridis")
library("janitor")

setwd("~/Documents/Data Science/Personal Project/ghg_data_analysis/")


files <- list.files(pattern = "\\.csv$", full.names = T)

# files
files <- files[-c(1,2)] # previously saved csv files from when script was original developed are removed.
# uncomment and run 'files' first before running this code.


ghg_data <- map_df(files, read_csv) %>% 
  clean_names() %>% 
  select(-2) %>% 
  rename("gas" = series_code,
         "region" = country_or_area,
         "emission_value" = value) %>%
  mutate_if(is.character, factor)


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
                                  valueBoxOutput("total_emission",
                                                 width = 4),
                                  
                                  box(
                                    width = 4,
                                    title = "Proportion of Gas Emission",
                                    plotlyOutput("piechart")
                                    ),
                                  
                                  box(
                                    title = "Top Emitting Countries",
                                    width = 4,
                                    status = "success",
                                    tableOutput("top_emitting_country")
                                    )
                                  ),
                                pickerInput("ghg",
                                            "Greenhouse gas",
                                            choices = unique(ghg_data$gas),
                                            options = list(`actions-box` = T),
                                            multiple = T),
                                
                                pickerInput("country", "Select Region or Country",
                                            choices = unique(ghg_data$region),
                                            options = list(`actions-box` = T),
                                            multiple = T)
                                ),

                            tabItem(
                              tabName = "comparison",
                              h2("Comparison Tool")
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
  
  output$total_emission <- renderValueBox(
    {
      valueBox(
        ghg_data %>% 
        filter(year == input$single_year & gas %in% input$single_gas) %>% 
        group_by(year, gas) %>% 
        summarize(total_emission = sum(emission_value)) %>% 
        pull(emission_value) %>% 
        sum() %>% 
        round(1) %>% 
        paste0(" KCO2e", sep = " "),
        "Total Emission",
        icon = icon("cloud_upload", lib = "glyphicon"),
        color = "olive"
        )
    
  }
  )
  
}

shinyApp(ui, server)
