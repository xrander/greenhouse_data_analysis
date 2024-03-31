setwd("/home/xrander/Documents/Data Science/Personal Project/ghg_data_analysis/emis-app-v2")

source("setup.r")
source("helper.r")

ui <- page_navbar(

## Theme set ---------------------------------------------------------------
  
  theme = bs_theme(
    bootswatch = "cyborg",
    version = 5
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
        #showcase =,
        #theme = ,
        max_height = "130px"
      ),
      value_box(
        title = "Year Emission",
        value = "holder_will",
        #showcase =,
        #theme = ,
        max_height = "130px"
      ),
      value_box(
        title = "Total Emission",
        value = total_emission(emis_tbl),
        #showcase =,
        #theme = ,
        max_height = "130px"
      ),
      pickerInput(
        inputId = "select_year",
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
      row_heights = c(2, 5.5, 8)
    )
  ),

## Nav_2--------------------------------
  nav_panel(
    title = "Comparison Tools",
    icon = bs_icon("activity")
  ),


## Nav_3 -----------------------------------------------------------------

  nav_panel(
    title = "Forecast",
    icon = bs_icon("alarm")
  ),


## Nav 4 -------------------------------------------------------------------

  nav_panel(
    title = "Scenario Analysis",
    icon = bs_icon("globe-europe-africa")
  )
)

server <- function(input, output) {
  
}

shinyApp(ui, server)

