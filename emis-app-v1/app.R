setwd("~/Documents/Data Science/Personal Project/ghg_data_analysis/emis-app-v1")

# Load Data ---------------------------------------------------------------

source("setup.r")

emis_tbl
forecast_mod

# UI Section --------------------------------------------------------------

## Header ------------------------------------------------------------------

header <- dashboardHeader(title = "Emission Tracker")

## Sidebar -----------------------------------------------------------------

sidebar <- dashboardSidebar(
  minified = TRUE,
  sidebarMenu(
    menuItem(
      "Dashboard", 
      tabName = "dashboard",
      icon = icon("dashboard", lib = "glyphicon")
    ),
    menuItem(
      "Comparison Tool",
      tabName = "comp-tool",
      icon = icon("code-compare", lib = "font-awesome")
    ),
    menuItem(
      "Predictive Analysis",
      tabName = "pred-anal",
      icon = icon("stats", lib = "glyphicon")
    ),
    menuItem(
      "Scenario Analysis",
      tabName = "sce-anal",
      icon = icon("arrows-spin", lib = "font-awesome")
    ),
    hr(),
    h3("About App:"),
    hr(),
    p(
      HTML(
        "This webapp aims to show</br> 
        the total emission of the various</br>
        greenhouse gases by countries. </br>
        The project data was collected </br>
        using the UNData API."
      )
    ),
    p(
      HTML(
          "There're tabs to compare emissions </br>
          across gases and countries"
      )
    ),
    p(
      HTML(
          "You can check emission forecast on </br>
          this webapp, and see emission </br>
          scenario simulations with a 2 </br>
          percent increase or decrease and </br>
          a 5 percent increase or decrease </br>
          in total emission."
      ) 
    )
  )
)

rightsidebar <- dashboardControlbar(collapsed = TRUE, skinSelector())
## App Body ----------------------------------------------------------------

body <- dashboardBody(

### First tab ---------------------------------------------------------------
  tabItems(
    tabItem(
      tabName = "dashboard",
      #### First Plot -------------------------
      box(
        title = strong("Greenhouse Gas Emission Trend (Country)"),
        width = 5,
        pickerInput(
          inputId = "country",
          label = "Country",
          choices = unique(emis_tbl$country),
          option = list(style = "btn-primary")
        ),
        materialSwitch(
          inputId = "log10_trans",
          label = "Logarithmic scale",
          value = FALSE,
          status = "primary"
        ),
        plotOutput(outputId = "country_gas")
      ),
      
      #### Second Plot ------------------------
      h3(strong("Top Emitting Countries"), align = "center")
    ),

### Second tab --------------------------------------------------------------
    tabItem(
      tabName = "comp-tool",
      h1(strong("Comparison Tool"), align = "right")
    ),

### Third tab ---------------------------------------------------------------
    tabItem(
      tabName = "pred-anal",
      h1("Predictive Analysis")
    ),

### Fourth Tab --------------------------------------------------------------
    tabItem(
      tabName = "sce-anal",
      h1("Scenario Analysis")
    )
  )
)


## Fourth Section ----------------------------------------------------------
footer <- dashboardFooter(
  left = "Developed and maintained by Olamide Adu",
  right = "Copenhagen, 2024"
)


## UI Combination ----------------------------------------------------------

ui <- dashboardPage(
  options = list(sidebarExpandonHover = FALSE),
  controlbar = rightsidebar,
  title = "Greenhouse Gas Emission Tracker",
  header = header,
  sidebar = sidebar,
  body = body,
  footer
)


# Functions and analysis for server side -----------------------------------------------

country_emis_plot <- function(data, region, log_transform) {
  p <- data |> 
    filter(country == region) |> 
    ggplot(aes(year, emission, group = gas, col = gas)) +
    geom_line(linewidth = 1.2) +
    scale_color_colorblind() +
    labs(x = "Year", y = "Emission GGCO2e") +
    theme_clean() +
    guides(color = guide_legend(title = "Green House Gas", nrow = 1, theme = theme_bw())) +
    theme(legend.position = "bottom", legend.text = element_text(face = "bold"))
  
  if (!log_transform) {
    p <- p + scale_y_log10(labels = label_log(base = 10))
  }
  return(p)
}

# Server Section ----------------------------------------------------------


server <- function(output, input) {
  reactive_plot <- reactive({
    req(input$country)
    country_emis_plot(emis_tbl, input$country, input$log10_trans == FALSE)
  })
  
  output$country_gas <- renderPlot({
    reactive_plot()
  })
}


# Shinyapp ----------------------------------------------------------------


shinyApp(ui, server)