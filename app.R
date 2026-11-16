# A&E Tracker: Shewhart chart analysis of NHS England and NHS Scotland A&E
# data. The functions used here are in the R/ folder, which Shiny loads before
# running this file.

library(shiny)
library(shinydashboard)
library(dplyr, warn.conflicts = FALSE)
library(stringr)

Sys.setenv(TZ = "Europe/London")
log_timing("app starting")

ae_data <- prepare_ae_data(read_ae_file("England"), read_ae_file("Scotland"))
organisations <- organisation_lookup(ae_data)
log_timing("data read and prepared")

ui <- app_ui(organisations)
log_timing("UI built")

shinyApp(
  # Warms up after the first page request (see R/startup.R)
  ui = warm_up_on_first_request(ui, ae_data),
  server = app_server(ae_data, organisations)
)
