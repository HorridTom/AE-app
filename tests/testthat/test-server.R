orgs <- organisation_lookup(ae_data_test)
server <- app_server(ae_data_test, orgs, end_date = as.Date("2026-11-02"))

# The charts drawn from the test data warn that 12 points are too few to form
# control limits; that doesn't matter for these tests.
set_inputs <- function(session, ...) suppressWarnings(session$setInputs(...))

test_that("The selection follows the inputs", {
  testServer(server, {
    set_inputs(
      session,
      country = "England",
      level = "National",
      t1_only_checkbox = FALSE,
      still_reporting_checkbox = TRUE,
      weekly_checkbox = FALSE
    )
    s <- selection()
    expect_equal(s$code, "E")
    expect_equal(s$measure, "All")
    expect_true(s$only_reporting)
    expect_false(s$weekly)

    set_inputs(
      session,
      level = "Provider",
      trust = "Chelsea And Westminster Hospital NHS Foundation Trust",
      t1_only_checkbox = TRUE
    )
    expect_equal(selection()$code, "RQM")
    expect_equal(selection()$measure, "Typ1")
  })
})

test_that("Options for the other country are ignored", {
  testServer(server, {
    # Weekly analysis ticked for Scotland, then England chosen
    set_inputs(
      session,
      country = "England",
      level = "Regional",
      region = "London",
      weekly_checkbox = TRUE,
      t1_only_checkbox = FALSE,
      still_reporting_checkbox = TRUE
    )
    expect_false(selection()$weekly)

    # Type 1 and "exclude providers" ticked for England, then Scotland chosen
    set_inputs(
      session,
      country = "Scotland",
      level = "Regional",
      regionScot = "Lothian",
      weekly_checkbox = TRUE,
      t1_only_checkbox = TRUE
    )
    s <- selection()
    expect_equal(s$code, "S08000024")
    expect_equal(s$measure, "All")
    expect_false(s$only_reporting)
    expect_true(s$weekly)
  })
})

test_that("Download file names describe the chart", {
  selection <- list(
    country = "England",
    level = "Provider",
    measure = "All",
    name = "Chelsea And Westminster Hospital NHS Foundation Trust"
  )
  expect_equal(
    download_file_name(
      selection,
      "PerfPlot",
      "2015-07-01",
      as.Date("2026-11-02")
    ),
    "Chelsea_And_Westminster_Hospital_PerfPlot_AllTypes_2015-07-01_to_2026-11-02.png"
  )

  selection <- list(
    country = "Scotland",
    level = "National",
    measure = "All",
    name = "Scotland"
  )
  expect_equal(
    download_file_name(
      selection,
      "AttendPlot",
      "2015-07-01",
      as.Date("2026-11-02")
    ),
    "Scotland_AttendPlot__2015-07-01_to_2026-11-02.png"
  )
})
