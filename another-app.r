source("data-model.r")

ui <- page_navbar(
  title = "EMT Tracker",
  nav_panel("dashboard"),
  nav_panel("comparison tools"),
  nav_panel("Forecast"),
  nav_panel("Scenario Analysis")
  )

server <- function(input, output) {
  
}

shinyApp(ui, server)