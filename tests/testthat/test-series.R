# Expected values are as published by NHS England and Public Health Scotland.

value_in <- function(series, period, column) {
  series[[column]][series$Period == as.Date(period)]
}

expect_counts <- function(series, period, total, within, over, ...) {
  expect_equal(value_in(series, period, "Total_Att"), total, ...)
  expect_equal(value_in(series, period, "Within_4h"), within, ...)
  expect_equal(value_in(series, period, "Greater_4h"), over, ...)
}

test_that("make_series() returns one row per month", {
  s <- make_series(ae_data_test, "Provider", "RQM")
  expect_s3_class(s, "data.frame")
  expect_equal(anyDuplicated(s$Period), 0)
  expect_true(all(
    c(
      "Code",
      "Name",
      "Nat_Code",
      "Period",
      "Within_4h",
      "Greater_4h",
      "Total_Att",
      "Total_Adm",
      "Performance",
      "days_in_period",
      "daily_ave_att",
      "daily_ave_adm"
    ) %in%
      names(s)
  ))
})

test_that("England provider series are correct (Chelsea and Westminster, Imperial)", {
  expect_counts(
    make_series(ae_data_test, "Provider", "RQM", "All"),
    "2017-04-01",
    23452,
    22059,
    1393
  )
  expect_counts(
    make_series(ae_data_test, "Provider", "RQM", "Typ1"),
    "2017-04-01",
    15957,
    14648,
    1309
  )
  expect_counts(
    make_series(ae_data_test, "Provider", "RYJ", "All"),
    "2017-09-01",
    23637,
    20577,
    3060
  )

  s <- make_series(ae_data_test, "Provider", "RQM", "All")
  expect_equal(value_in(s, "2017-04-01", "Total_Adm"), 3375)
  expect_equal(value_in(s, "2017-04-01", "days_in_period"), 30)
  expect_equal(value_in(s, "2017-04-01", "daily_ave_att"), 23452 / 30)
})

test_that("England regional and national series are correct", {
  expect_counts(
    make_series(ae_data_test, "Regional", "Lo", "All"),
    "2017-09-01",
    395373,
    356603,
    38770
  )
  expect_counts(
    make_series(ae_data_test, "National", "E", "All"),
    "2017-09-01",
    1925961,
    1726934,
    199027
  )
})

test_that("only_reporting leaves out providers with missing 4-hour data", {
  all_provs <- make_series(
    ae_data_test,
    "National",
    "E",
    only_reporting = FALSE
  )
  reporting <- make_series(ae_data_test, "National", "E", only_reporting = TRUE)
  both <- inner_join(
    all_provs,
    reporting,
    by = "Period",
    suffix = c("_all", "_reporting")
  )
  expect_true(all(both$Total_Att_reporting <= both$Total_Att_all))
  expect_true(any(both$Total_Att_reporting < both$Total_Att_all))
  expect_false(anyNA(reporting$Within_4h))
})

# Monthly Scottish figures are apportioned from weeks, so allow a difference
# of up to 20 attendances (the published figures have also been revised
# slightly since these values were first recorded). Weekly figures are as
# published.
test_that("Scotland monthly series are correct", {
  expect_counts(
    make_series(ae_data_test, "Provider", "A210H"),
    "2017-09-01",
    3156,
    2889,
    267,
    tolerance = 20,
    scale = 1
  )
  expect_counts(
    make_series(ae_data_test, "Regional", "S08000031"),
    "2017-09-01",
    30070,
    27042,
    3028,
    tolerance = 20,
    scale = 1
  )
  expect_counts(
    make_series(ae_data_test, "National", "S"),
    "2017-09-01",
    115617,
    106880,
    8737,
    tolerance = 20,
    scale = 1
  )
})

test_that("Scotland weekly series are correct", {
  expect_counts(
    make_series(ae_data_test, "National", "S", weekly = TRUE),
    "2017-01-01",
    25164,
    23238,
    1926
  )
  expect_counts(
    make_series(ae_data_test, "Regional", "S08000031", weekly = TRUE),
    "2017-01-01",
    6528,
    5689,
    839
  )
  s <- make_series(ae_data_test, "Provider", "A210H", weekly = TRUE)
  expect_counts(s, "2017-01-01", 894, 874, 20)
  expect_true(all(s$days_in_period == 7))
})

test_that("Scotland has no type 1 breakdown, so measure is ignored", {
  expect_equal(
    make_series(ae_data_test, "National", "S", "Typ1"),
    make_series(ae_data_test, "National", "S", "All")
  )
})
