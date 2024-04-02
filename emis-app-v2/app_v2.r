setwd("/home/xrander/Documents/Data Science/Personal Project/ghg_data_analysis/emis-app-v2")

source("setup.r")
source("helper.r")

ui <- page_navbar(

## Theme set ---------------------------------------------------------------

   theme = bs_theme(
     bootswatch = "shiny",
     version = 5,
     success = "lightgreen",
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
      value_box(
        title = "Emission",
        textOutput("year_emission"),
        #showcase =,
        #theme = ,
        max_height = "140px"
      ),
      value_box(
        title = "Total Emission",
        value = paste0(total_emission(emis_tbl), "M ", "GGCO2e"),
        showcase = bs_icon("clouds-fill"),
        #theme = ,
        max_height = "140px"
      ),
      pickerInput(
        inputId = "specific_year",
        label = "Select Year",
        choices = unique(emis_tbl$year),
        selected = unique(emis_tbl$year)[7],
        options = list(style = "btn-success")
      ),
      card(
        title = "Trends of gases",
        max_height = "350px",
        value_box(
          title = "Percentage change",
          value = "hold",
          theme = "bg-warning" ,
          max_height = "100px"
        ),
        card_body(
          max_height = "250px",
          p("hold")
        )
      ),
      card(
        title = "Top Emitting Region",
        max_height = "350px",
        card_body(
          p("Emissions in the regions")
        )
      ),
      card(
        title = "Greenhouse Gas Proportion Monitor",
        min_height = "300px",
        max_height = "350px"
      ),
      card(), # For map
      col_widths = c(3, 3, 3, 3, # first row
                     4, 4, 4, # second row
                     12),
      row_heights = c(3, 5.5, 8)
    )
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
        
        ## Space --------------------------------
        nav_spacer(),
        
        ## Subtab-2 ----------------------------------
        nav_panel(
          title = "Scenario Analysis",
          icon = bs_icon("globe-europe-africa")
        )
      )
      
    )

)
server <- function(input, output, session) {
  output$year_emission <- renderText({
    ## Fix this
    yearly_emission(emis_tbl, input$specific_year)
  })
}

shinyApp(ui, server)

