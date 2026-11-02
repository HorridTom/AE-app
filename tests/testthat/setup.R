# Load the app's packages and functions (R/ folder) for the tests
library(shiny)
library(shinydashboard)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
shiny::loadSupport(test_path("..", ".."), renv = globalenv())

# Data prepared by prepare_ae_data(): all of England for 2017, Chelsea and
# Westminster (RQM) from June 2015 to September 2026, and Scotland from late
# December 2016 to early January 2018. Made by make_test_data.R.
ae_data_test <- readRDS(test_path("test-data", "ae_data_test.rds"))
