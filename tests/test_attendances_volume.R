context("Plot attendances volume")
source('../perf_4h_analysis.R')
source("../spc_rules.R")

load('test-data/AE_data_test.rda')

AE_data_test <- clean_region_col(AE_data_test)

testthat::test_that("Attendances volume plot is correct for April 2017 for Chelsea and Westminster",{
  vchart_all <- plot_volume(df = AE_data_test, code = 'RQM', brk.date = NULL, measure = 'All',
                            start.date = "2014-01-01", end.date = "2017-06-30",
                            weeklyOrMonthly = "monthly")
  vchart_typ1 <- plot_volume(df = AE_data_test, code = 'RQM', brk.date = NULL, measure = 'Typ1',
                             start.date = "2014-01-01", end.date = "2017-06-30",
                             weeklyOrMonthly = "monthly")
  

#check correct chart title for ggplot
testthat::expect_match(vchart_all$labels$title,"attendances")
testthat::expect_match(vchart_all$labels$title,"month")
testthat::expect_match(vchart_typ1$labels$title, "attendances")
testthat::expect_match(vchart_typ1$labels$title, "month")

#check correct y-axis label for ggplot
testthat::expect_match(vchart_all$labels$y, "attendances")
testthat::expect_match(vchart_typ1$labels$y, "attendances")

#check correct subtitle label for ggplot
testthat::expect_match(vchart_all$labels$subtitle, "Chelsea")
testthat::expect_match(vchart_all$labels$subtitle, "All")
testthat::expect_match(vchart_typ1$labels$subtitle, "Chelsea ")
testthat::expect_match(vchart_typ1$labels$subtitle,"Type 1")

})





