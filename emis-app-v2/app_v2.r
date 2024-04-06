setwd("/home/xrander/Documents/Data Science/Personal Project/ghg_data_analysis/emis-app-v2")

source("setup.r")
source("helper-1.r")

ui <- page_navbar(

## Theme set ---------------------------------------------------------------

   theme = bs_theme(
     bootswatch = "bootstrap",
     version = 5,
     success = "lightgreen",
     danger = "orange",
     "text-danger" = "darkred",
     #"table-color" = "#86C7ED",
     base_font = font_google("Lato")
   ),

## Title -------------------------------------------------------------------

  title = "EMT Tracker",

  

# Navigation section -------------------------------------------------------

## Nav_1 --------------------------------------------------------------------
  nav_panel(
    title = "Overview",
    icon = bs_icon("speedometer2"),
  
    # Nav bar content
    layout_columns(
      
      value_box(
        title = "Regions Tracked",
        value = total_regions_monitored(emis_tbl),
        showcase = bs_icon("map-fill"),
        p("All nations within the European union
          are not represented", 
          bs_icon("emoji-grimace"),
          "only the Union is presented"),
        theme = "success",
        max_height = "140px"
      ),
      
      uiOutput("year_emission_vb"),
      
      value_box(
        title = "Emissions to date",
        value = paste0(total_emission(emis_tbl), "Billion ", "GGCO2e"),
        showcase = bs_icon("clouds"),
        theme = "danger",
        max_height = "140px"
      ),
      uiOutput("percent_year_vb"),
      
      noUiSliderInput(
        inputId = "specific_year",
        label = "Year",
        min = min(as.integer(emis_tbl$year)),
        max = max(as.integer(emis_tbl$year)),
        step = 1,
        value = 2021,
        orientation = "vertical",
        width = "30px",
        tooltips = TRUE,
        height = "100px",
        direction = "rtl"
      ),
      
      card(
        card_body(
          max_height = "350px",
          plotlyOutput("percent_change_country")
        )
      ),
      plotOutput("top_emitter"),
      plotOutput("least_emitter"),
      card(
        plotlyOutput("plot_gas_prop"),
        theme = "secondary",
        max_height = "400px"
      ),
      card(), 
      col_widths = c(3, 3, 3, 2, 1, # first row
                     4, 4, 4, # second row
                     4, 8),
      row_heights = c(3, 5.5, 8)
    ),

  ),

## Nav_2--------------------------------
  nav_panel(
    title = "Comparison Tools",
    icon = bs_icon("activity"),

    # Nav bar content ---------------------------------------------------------
    layout_columns(
      card(
        title = "Emission Per Person",
        max_height = "220px"
      ),
      value_box(
        title = "vs-emission",
        value = "hold",
        theme = "bg-warning",
        max_height = "120px"
      ),
      card(
        title = "Radar Chart",
        max_height = "220px"
      ),
      card(),
      card(),
      col_widths = c(5, 2, 5, 8, 4)
    )
  ),


## Nav_3 -----------------------------------------------------------------
  nav_panel(
      title = "Forecast",
      icon = bs_icon("alarm"),
      
      navset_tab(
        ## Subtab-1 -----------------------------------------------
        nav_panel(
          title = "Forecast",
          icon = bs_icon("alarm")
        ),
        
        ## Subtab-2 ----------------------------------
        nav_panel(
          title = "Scenario Analysis",
          icon = bs_icon("globe-europe-africa")
        )
      )
      
    )

)
server <- function(input, output, session) {
  
  top_region <- reactive({
    top_5_region_emission(emis_tbl, input$specific_year)
  })
  
  least_region <- reactive({
    least_5_emitter(emis_tbl, input$specific_year)
  })
  
  yearly_emis <- reactive({
    yearly_emission(emis_tbl, input$specific_year)
  })
  
  percent_year <- reactive({
     percent_change_year(emis_tbl, input$specific_year) |> 
      pull(percent_change)
  })
  
  percent_country <- reactive({
    percent_change_country(emis_tbl, input$specific_year)
  })
  
  percent_status <- reactive({
    percent_change_year(emis_tbl, input$specific_year) |> 
      pull(status)
  })
  
  year_gas_prop <- reactive({
    gas_prop(emis_tbl, input$specific_year)
  })
  
  output$year_emission_vb <- renderUI({
    value_box(
      title = paste0("Year ",  input$specific_year, " Total Emission"),
      value = yearly_emis(),
      showcase = bs_icon("wind"),
      theme = "danger",
      max_height = "140px"
    )
  })
  
  
  output$percent_year_vb <- renderUI({
    value_box(
      title = "Percentage change",
      value = paste0(percent_year(), "%"),
      theme = "success",
      showcase = if(percent_status() == "bad") {
        bs_icon("arrow-up-circle-fill", class = "text-danger")
      } else
        bs_icon("arrow-down-circle-fill", class = "text-success"),
      showcase_layout = "top right",
      max_height = "140px"
    )
  })
  
  output$percent_change_country <- renderPlotly({
    plot_yearly_change(percent_country())
  })
  
  output$top_emitter <- renderPlot({
    plot_top_emitter(top_region())
  })
  
  output$least_emitter <- renderPlot({
    plot_least_emitter(least_region())
  })
  
  output$plot_gas_prop <- renderPlotly({
    plot_gas_prop(year_gas_prop())
  })
}

shinyApp(ui, server)

