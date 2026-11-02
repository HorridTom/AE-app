context("Plot performance and volume")
source('../perf_4h_analysis.R')
source("../spc_rules.R")

# Chelsea and Westminster (RQM), July 2015 to September 2026. Made by
# make_test_data.R.
load('test-data/AE_data_test_rqm.rda')

# The control limits below are regression values: they are what autospc
# 0.2.0.9002 gives, and check that the charts don't change unintentionally.
# If a new version of autospc changes them on purpose, check the new charts
# and update the values.

chart_row <- function(chart, month) {
  d <- as.data.frame(chart)
  d[d$x == as.Date(month), ]
}

perf_chart <- function(measure) {
  plot_performance(df = AE_data_test_rqm, code = 'RQM', measure = measure,
                   start.date = "2015-07-01", end.date = "2026-09-30",
                   level = "Provider", weeklyOrMonthly = "Monthly",
                   onlyProvsReporting = FALSE)
}

volume_chart <- function(measure, attOrAdm) {
  plot_volume(df = AE_data_test_rqm, code = 'RQM', measure = measure,
              start.date = "2015-07-01", end.date = "2026-09-30",
              level = "Provider", weeklyOrMonthly = "Monthly",
              onlyProvsReporting = FALSE, attOrAdm = attOrAdm)
}

testthat::test_that("Performance plot is correct for Chelsea and Westminster", {
  ppchart_all <- perf_chart("All")
  ppchart_typ1 <- perf_chart("Typ1")

  testthat::expect_s3_class(ppchart_all, "ggplot")
  testthat::expect_s3_class(ppchart_typ1, "ggplot")

  # Exactly one point for April 2017
  testthat::expect_equal(nrow(chart_row(ppchart_all, "2017-04-01")), 1)
  testthat::expect_equal(nrow(chart_row(ppchart_typ1, "2017-04-01")), 1)

  # Percentage within 4 hours, April 2017 (from the published counts)
  testthat::expect_equal(chart_row(ppchart_all, "2017-04-01")$series,
                         100 * 22059 / 23452, tolerance = 1e-6)
  testthat::expect_equal(chart_row(ppchart_typ1, "2017-04-01")$series,
                         100 * 14648 / 15957, tolerance = 1e-6)

  # Centre line and limits, April 2017 and April 2024
  apr17 <- chart_row(ppchart_all, "2017-04-01")
  apr24 <- chart_row(ppchart_all, "2024-04-01")
  testthat::expect_equal(c(apr17$cl, apr17$ucl, apr17$lcl),
                         c(94.39386, 96.20126, 92.58646), tolerance = 1e-5)
  testthat::expect_equal(c(apr24$cl, apr24$ucl, apr24$lcl),
                         c(79.43409, 84.04628, 74.82190), tolerance = 1e-5)

  apr17 <- chart_row(ppchart_typ1, "2017-04-01")
  testthat::expect_equal(c(apr17$cl, apr17$ucl, apr17$lcl),
                         c(92.44618, 95.59137, 89.30099), tolerance = 1e-5)
})

testthat::test_that("Volume plots are correct for Chelsea and Westminster", {
  vchart_all <- volume_chart("All", "Attendances")
  vchart_typ1 <- volume_chart("Typ1", "Attendances")
  achart_all <- volume_chart("All", "Admissions")

  testthat::expect_s3_class(vchart_all, "ggplot")
  testthat::expect_s3_class(achart_all, "ggplot")

  # Average daily attendances, April 2017 (30 days), rounded for the C' chart
  testthat::expect_equal(chart_row(vchart_all, "2017-04-01")$series,
                         round(23452 / 30))
  testthat::expect_equal(chart_row(vchart_typ1, "2017-04-01")$series,
                         round(15957 / 30))
  testthat::expect_equal(chart_row(achart_all, "2017-04-01")$series,
                         round(3375 / 30))

  apr24 <- chart_row(vchart_all, "2024-04-01")
  testthat::expect_equal(c(apr24$cl, apr24$ucl, apr24$lcl),
                         c(887.0952, 1045.5526, 728.6379), tolerance = 1e-5)
  apr24 <- chart_row(achart_all, "2024-04-01")
  testthat::expect_equal(c(apr24$cl, apr24$ucl, apr24$lcl),
                         c(85.72222, 95.0276, 76.41684), tolerance = 1e-5)
})
