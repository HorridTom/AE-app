# A&E Tracker: Shewhart chart analysis of NHS England and NHS Scotland A&E
# data. The functions used here are in the R/ folder, which Shiny loads before
# running this file.

library(shiny)
library(shinydashboard)
library(dplyr, warn.conflicts = FALSE)
library(stringr)

Sys.setenv(TZ = "Europe/London")
log_timing("app starting")

england <- read_ae_file("England")
log_timing("England data read")
scotland <- read_ae_file("Scotland")
log_timing("Scotland data read")
ae_data <- prepare_ae_data(england, scotland)
rm(england, scotland)
organisations <- organisation_lookup(ae_data)
log_timing("data prepared")

ui <- app_ui(organisations)
log_timing("UI built")

shinyApp(
  # Warms up after the first page request (see R/startup.R)
  ui = warm_up_on_first_request(ui, ae_data),
  server = app_server(ae_data, organisations)
)
