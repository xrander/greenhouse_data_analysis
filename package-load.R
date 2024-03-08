if (!require("pacman", quietly = T)) {
  install.packages("pacman")
  library(pacman)
} else(
  library(pacman)
)

p_load(tidyverse, plotly, hrbrthemes, ggthemes, janitor,
               scales, httr, jsonlite)
