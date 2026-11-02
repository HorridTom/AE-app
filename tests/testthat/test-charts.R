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

  # Average daily attendances, April 2017 (30 days), rounded for the C' chart
  expect_equal(chart_row(attendances, "2017-04-01")$series, round(23452 / 30))
  expect_equal(chart_row(type1, "2017-04-01")$series, round(15957 / 30))
  expect_equal(chart_row(admissions, "2017-04-01")$series, round(3375 / 30))

  apr24 <- chart_row(attendances, "2024-04-01")
  expect_equal(
    c(apr24$cl, apr24$ucl, apr24$lcl),
    c(887.0952, 1045.5526, 728.6379),
    tolerance = 1e-5
  )
  apr24 <- chart_row(admissions, "2024-04-01")
  expect_equal(
    c(apr24$cl, apr24$ucl, apr24$lcl),
    c(85.72222, 95.0276, 76.41684),
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

test_that("A provider with no admissions gives an error, not a chart", {
  s <- make_series(ae_data_test, "Provider", "RQM")
  s$Total_Adm <- 0
  expect_error(
    chart_volume(s, "Provider", volume = "Admissions"),
    "No admissions data"
  )
})
