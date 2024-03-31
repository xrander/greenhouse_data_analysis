if (!require("pacman", quietly = T)) {
  install.packages("pacman")
  library(pacman)
} else(
  library(pacman)
)

p_load(tidyverse, plotly, hrbrthemes, ggthemes, rvest, janitor,
       scales, httr, jsonlite, shiny, shinydashboard, shinydashboardPlus,
       shinyWidgets, leaflet, tidymodels, modeltime, timetk, digest, bslib, bsicons)
