context("Plot performance and volume")
source('../perf_4h_analysis.R')
source("../spc_rules.R")

load('test-data/AE_data_test.rda')

testthat::test_that("Performance plot is correct for April 2017 for Chelsea and Westminster",{
  ppchart_all <- plot_performance(df = AE_data_test, prov_codes = c('RQM'), brk.date = NULL, measure = 'All',
                                  start.date = "2014-01-01", end.date = "2017-06-30")
  ppchart_typ1 <- plot_performance(df = AE_data_test, prov_codes = c('RQM'), brk.date = NULL, measure = 'Typ1',
                                   start.date = "2014-01-01", end.date = "2017-06-30")
  
  # Check output is a ggplot
  testthat::expect_is(ppchart_all, "ggplot")
  testthat::expect_is(ppchart_typ1, "ggplot")
  
  # Check exactly one x value for April 2017
  Apr2017 <- as.Date("2017-04-01", tz = "Europe/London")
  
  testthat::expect_equal(sum(ppchart_all$data$x == Apr2017), 1)
  testthat::expect_equal(sum(ppchart_typ1$data$x == Apr2017), 1)
  
  # Check correct performance for April 2017
  testthat::expect_equal(ppchart_all$data$y[which(ppchart_all$data$x == Apr2017)],
                         94.06021, tolerance = .001, scale = 1)
  testthat::expect_equal(ppchart_typ1$data$y[which(ppchart_typ1$data$x == Apr2017)],
                         91.79670, tolerance = .001, scale = 1)
  
  # Check correct centre line for April 2017
  testthat::expect_equal(ppchart_all$data$cl[which(ppchart_all$data$x == Apr2017)],
                         91.70907, tolerance = .001, scale = 1)
  testthat::expect_equal(ppchart_typ1$data$cl[which(ppchart_typ1$data$x == Apr2017)],
                         88.3698, tolerance = .001, scale = 1)
  
  # Check correct upper control limit for April 2017
  testthat::expect_equal(ppchart_all$data$ucl[which(ppchart_all$data$x == Apr2017)],
                         98.07893, tolerance = .001, scale = 1)
  testthat::expect_equal(ppchart_typ1$data$ucl[which(ppchart_typ1$data$x == Apr2017)],
                         97.57907, tolerance = .001, scale = 1)
  
  # Check correct lower control limit for April 2017
  testthat::expect_equal(ppchart_all$data$lcl[which(ppchart_all$data$x == Apr2017)],
                         85.33921, tolerance = .001, scale = 1)
  testthat::expect_equal(ppchart_typ1$data$lcl[which(ppchart_typ1$data$x == Apr2017)],
                         79.16053, tolerance = .001, scale = 1)
  
})

testthat::test_that("Volume plot is correct for April 2017 for Chelsea and Westminster",{
  vchart_all <- plot_volume(df = AE_data_test, prov_codes = c('RQM'), brk.date = NULL, measure = 'All',
                            start.date = "2014-01-01", end.date = "2017-06-30")
  vchart_typ1 <- plot_volume(df = AE_data_test, prov_codes = c('RQM'), brk.date = NULL, measure = 'Typ1',
                             start.date = "2014-01-01", end.date = "2017-06-30")
  
  # Check output is a ggplot
  testthat::expect_is(vchart_all, "ggplot")
  testthat::expect_is(vchart_typ1, "ggplot")
  
  # Check exactly one x value for April 2017
  Apr2017 <- as.Date("2017-04-01", tz = "Europe/London")
  
  testthat::expect_equal(sum(vchart_all$data$x == Apr2017), 1)
  testthat::expect_equal(sum(vchart_typ1$data$x == Apr2017), 1)
  
  # Check correct performance for April 2017
  testthat::expect_equal(vchart_all$data$y[which(vchart_all$data$x == Apr2017)],
                         23452, tolerance = .001, scale = 1)
  testthat::expect_equal(vchart_typ1$data$y[which(vchart_typ1$data$x == Apr2017)],
                         15957, tolerance = .001, scale = 1)
  
  # Check correct centre line at April 2017
  testthat::expect_equal(vchart_all$data$cl[which(vchart_all$data$x == Apr2017)],
                         24076.5, tolerance = .001, scale = 1)
  testthat::expect_equal(vchart_typ1$data$cl[which(vchart_typ1$data$x == Apr2017)],
                         16553.16667, tolerance = .001, scale = 1)
  
  # Check correct upper control limit at April 2017
  testthat::expect_equal(vchart_all$data$ucl[which(vchart_all$data$x == Apr2017)],
                         31056.28723, tolerance = .001, scale = 1)
  testthat::expect_equal(vchart_typ1$data$ucl[which(vchart_typ1$data$x == Apr2017)],
                         21838.80496, tolerance = .001, scale = 1)
  
  # Check correct lower control limit at April 2017
  testthat::expect_equal(vchart_all$data$lcl[which(vchart_all$data$x == Apr2017)],
                         17096.71277, tolerance = .001, scale = 1)
  testthat::expect_equal(vchart_typ1$data$lcl[which(vchart_typ1$data$x == Apr2017)],
                         11267.52837, tolerance = .001, scale = 1)
  
})
