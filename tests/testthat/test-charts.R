# The control limits below are regression values: they are what autospc
# 0.2.0.9002 gives, and check that the charts don't change unintentionally.
# If a new version of autospc changes them on purpose, check the new charts
# and update the values.

chart_row <- function(chart, month) {
  d <- as.data.frame(chart)
  d[d$x == as.Date(month), ]
}

rqm <- function(measure) make_series(ae_data_test, "Provider", "RQM", measure)

perf_chart <- function(measure) {
  chart_performance(rqm(measure), "Provider", measure, end_date = "2026-09-30")
}

volume_chart <- function(measure, volume) {
  chart_volume(
    rqm(measure),
    "Provider",
    measure,
    volume = volume,
    end_date = "2026-09-30"
  )
}

test_that("Performance chart is correct for Chelsea and Westminster", {
  all_types <- perf_chart("All")
  type1 <- perf_chart("Typ1")

  expect_s3_class(all_types, "ggplot")
  expect_equal(nrow(chart_row(all_types, "2017-04-01")), 1)

  # Percentage within 4 hours, April 2017 (from the published counts)
  expect_equal(
    chart_row(all_types, "2017-04-01")$series,
    100 * 22059 / 23452,
    tolerance = 1e-6
  )
  expect_equal(
    chart_row(type1, "2017-04-01")$series,
    100 * 14648 / 15957,
    tolerance = 1e-6
  )

  apr17 <- chart_row(all_types, "2017-04-01")
  apr24 <- chart_row(all_types, "2024-04-01")
  expect_equal(
    c(apr17$cl, apr17$ucl, apr17$lcl),
    c(94.39386, 96.20126, 92.58646),
    tolerance = 1e-5
  )
  expect_equal(
    c(apr24$cl, apr24$ucl, apr24$lcl),
    c(79.43409, 84.04628, 74.82190),
    tolerance = 1e-5
  )

  apr17 <- chart_row(type1, "2017-04-01")
  expect_equal(
    c(apr17$cl, apr17$ucl, apr17$lcl),
    c(92.44618, 95.59137, 89.30099),
    tolerance = 1e-5
  )
})

test_that("Volume charts are correct for Chelsea and Westminster", {
  attendances <- volume_chart("All", "Attendances")
  type1 <- volume_chart("Typ1", "Attendances")
  admissions <- volume_chart("All", "Admissions")

  expect_s3_class(attendances, "ggplot")
  expect_s3_class(admissions, "ggplot")

  # U' charts: the plotted value is the total over the number of days
  apr17 <- chart_row(attendances, "2017-04-01")
  expect_equal(apr17$y, 23452)
  expect_equal(apr17$n, 30)
  expect_equal(apr17$series, 23452 / 30)
  expect_equal(chart_row(type1, "2017-04-01")$series, 15957 / 30)
  expect_equal(chart_row(admissions, "2017-04-01")$series, 3375 / 30)

  # The limits vary with the number of days in the month: narrower for
  # March (31 days) than February 2024 (29 days)
  expect_lt(
    chart_row(attendances, "2024-03-01")$ucl,
    chart_row(attendances, "2024-02-01")$ucl
  )

  apr24 <- chart_row(attendances, "2024-04-01")
  expect_equal(
    c(apr24$cl, apr24$ucl, apr24$lcl),
    c(878.7422, 1022.5893, 734.8951),
    tolerance = 1e-5
  )
  apr24 <- chart_row(admissions, "2024-04-01")
  expect_equal(
    c(apr24$cl, apr24$ucl, apr24$lcl),
    c(85.75137, 94.95621, 76.54653),
    tolerance = 1e-5
  )
})

test_that("Chart titles describe the selection", {
  p <- perf_chart("All")
  expect_equal(
    p$labels$subtitle,
    "Chelsea And Westminster Hospital NHS Foundation Trust\nAll department types"
  )

  s <- make_series(ae_data_test, "Regional", "Lo")
  p <- chart_volume(
    s,
    "Regional",
    only_reporting = TRUE,
    end_date = "2017-12-31"
  )
  expect_equal(p$labels$title, "Average daily A&E attendances per month")
  expect_equal(
    p$labels$subtitle,
    paste0(
      "Region:London\nAll department types\n",
      "Includes only providers that are still reporting"
    )
  )

  s <- make_series(ae_data_test, "Regional", "S08000031", weekly = TRUE)
  p <- chart_volume(s, "Regional", weekly = TRUE, end_date = "2017-12-31")
  expect_equal(p$labels$title, "Average daily A&E attendances per week")
  expect_equal(p$labels$subtitle, "Board:Greater Glasgow and Clyde")
})

# Where there is no chart, chart functions stop with a "no_chart" condition,
# whose message the app shows instead.

test_that("No admissions gives a message, not a chart", {
  s <- rqm("All")
  s$Total_Adm <- 0
  expect_error(
    chart_volume(s, "Provider", volume = "Admissions"),
    "No emergency admissions via A&E are reported",
    class = "no_chart"
  )
})

test_that("Type 1 for a provider with no type 1 department gives a message", {
  s <- rqm("Typ1")
  s$Total_Att <- 0
  s$Within_4h <- 0
  s$Greater_4h <- 0
  expected <- "There are no type 1 \\(major A&E department\\) attendances"
  expect_error(
    chart_performance(s, "Provider", "Typ1"),
    expected,
    class = "no_chart"
  )
  expect_error(
    chart_volume(s, "Provider", "Typ1", volume = "Attendances"),
    expected,
    class = "no_chart"
  )
})

test_that("Performance always 100% is charted, with a note explaining it", {
  s <- rqm("All")
  s$Within_4h <- s$Total_Att
  s$Greater_4h <- 0
  chart <- suppressWarnings(
    chart_performance(s, "Provider", end_date = "2026-09-30")
  )
  expect_s3_class(chart, "ggplot")
  expect_true(all(as.data.frame(chart)$series == 100))
  expect_equal(
    all_within_4h_note(s, end_date = "2026-09-30"),
    paste(
      "Every attendance was within 4 hours in every month from July 2015",
      "to September 2026, so there is no variation from which to calculate",
      "control limits."
    )
  )
  expect_equal(all_within_4h_note(rqm("All"), end_date = "2026-09-30"), "")
})

test_that("No data in the period gives a message saying when the data ends", {
  # Scotland: the last week, even where weeks are apportioned to months
  s <- make_series(ae_data_test, "Provider", "A210H")
  expect_error(
    chart_volume(
      s,
      "Provider",
      start_date = "2018-02-01",
      end_date = "2026-09-30"
    ),
    "The latest data is for the week ending 7 January 2018.",
    fixed = TRUE,
    class = "no_chart"
  )

  expect_error(
    chart_volume(
      rqm("All"),
      "Provider",
      start_date = "2026-10-01",
      end_date = "2026-12-31"
    ),
    paste(
      "between October 2026 and December 2026.",
      "The latest data is for September 2026."
    ),
    fixed = TRUE,
    class = "no_chart"
  )
})

test_that("Periods with no attendances are left out of the performance chart", {
  # e.g. a type 1 breach reported with no type 1 attendances
  s <- rqm("All")
  s$Total_Att[s$Period == as.Date("2017-04-01")] <- 0
  s$Within_4h[s$Period == as.Date("2017-04-01")] <- -1
  chart <- chart_performance(s, "Provider", end_date = "2026-09-30")
  expect_equal(nrow(chart_row(chart, "2017-04-01")), 0)
  expect_equal(nrow(chart_row(chart, "2017-05-01")), 1)
})
