# Makes test-data/ae_data_test.rds for the tests. Run from the app's folder,
# with a local copy of the AE-data repository:
#
#   Sys.setenv(AE_DATA_SOURCE = "~/Rprojects/AE-data/data")
#   source("tests/testthat/make_test_data.R")

library(dplyr, warn.conflicts = FALSE)
library(stringr)
shiny::loadSupport(".", renv = globalenv())

ae_data <- prepare_ae_data(read_ae_file("England"), read_ae_file("Scotland"))

ae_data_test <- ae_data |>
  filter(
    (Country == "England" & format(Period, "%Y") == "2017") |
      (Prov_Code == "RQM" & Period <= as.Date("2026-09-01")) |
      (Country == "Scotland" &
        Period >= as.Date("2016-12-25") &
        Period <= as.Date("2018-01-07"))
  )

saveRDS(
  ae_data_test,
  file.path("tests", "testthat", "test-data", "ae_data_test.rds")
)
