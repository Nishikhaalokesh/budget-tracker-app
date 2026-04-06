# ============================================================
# FILE: app.R
# PURPOSE: Launch file for shinyapps.io deployment
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(scales)

source("R/functions_app.R")

ui     <- source("app/ui_app.R",     local=TRUE)$value
server <- source("app/server_app.R", local=TRUE)$value

shinyApp(ui=ui, server=server)
