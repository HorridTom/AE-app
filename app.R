# A&E Tracker: Shewhart chart analysis of NHS England and NHS Scotland A&E
# data. The functions used here are in the R/ folder, which Shiny loads before
# running this file.

library(shiny)
library(shinydashboard)
library(dplyr, warn.conflicts = FALSE)
library(stringr)

Sys.setenv(TZ = "Europe/London")

ae_data <- prepare_ae_data(read_ae_file("England"), read_ae_file("Scotland"))
organisations <- organisation_lookup(ae_data)

shinyApp(
  ui = app_ui(organisations),
  server = app_server(ae_data, organisations)
)
