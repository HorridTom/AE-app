# Builds tests/test-data/AE_data_test_scot.rda: Public Health Scotland weekly
# data for 2017, in the form the app uses (after standardise_data()), for the
# tests in test_make_perf_series.R. Run from the tests folder with a local copy
# of the AE-data repository:
#
#   AE_DATA_SOURCE=~/Rprojects/AE-data/data Rscript make_test_data.R

suppressMessages(library(dplyr))
source("../perf_4h_analysis.R")
source("../data_loading.R")

AE_data_test_scot <- read_ae_file("Scotland") %>%
  filter(Week_End >= as.Date("2016-12-25"), Week_End <= as.Date("2018-01-07")) %>%
  standardise_data()

save(AE_data_test_scot, file = file.path("test-data", "AE_data_test_scot.rda"))

# Chelsea and Westminster (RQM), July 2015 to September 2026, for the chart
# tests in test_perf_plot.R: long enough for autospc to form control limits.
AE_data_test_rqm <- read_ae_file("England") %>%
  filter(Prov_Code == "RQM", Month_Start <= as.Date("2026-09-01")) %>%
  clean_region_col()

save(AE_data_test_rqm, file = file.path("test-data", "AE_data_test_rqm.rda"))
