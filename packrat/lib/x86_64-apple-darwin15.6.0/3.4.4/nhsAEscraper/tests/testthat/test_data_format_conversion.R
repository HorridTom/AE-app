library(nhsAEscraper)
context("Convert imported data to a standard format")

rawDataList <- load_AE_files(directory = 'test-data/')
rawDataList_conv <- lapply(rawDataList, delete_extra_columns)

test_that("Superfluous columns correctly deleted",{

  comparison_1 <- compare::compare(rawDataList[[1]], rawDataList_conv[[1]])
  comparison_2 <- compare::compare(rawDataList[[2]], rawDataList_conv[[2]])

  expect_equal(comparison_1$result, TRUE, info = 'does not mess up data with correct columns')
  expect_equal(comparison_2$result, FALSE, info = 'does change data with incorrect columns')

  expect_match(rawDataList_conv[[1]][[1]], "Code", all = FALSE, info = 'correct first column heading present (still correct)')
  expect_match(rawDataList_conv[[2]][[1]], "Code", all = FALSE, info = 'correct first column heading present (after change)')
  expect_match(rawDataList_conv[[1]][[21]], "Number of patients spending >12 hours", all = FALSE, info = 'correct last column heading present (still correct)')
  expect_match(rawDataList_conv[[2]][[21]], "Number of patients spending >12 hours", all = FALSE, info = 'correct last column heading present (after change)')

})

test_that("Pick up incorrect formats",{
  expect_equal(check_format(rawDataList[[1]]), TRUE, info = 'valid format detected correctly')
  expect_equal(check_format(rawDataList[[2]]), FALSE, info = 'invalid format detected correctly')
  expect_equal(check_format(rawDataList_conv[[2]]), TRUE, info = 'valid format (post-column-correction) detected correctly')
})

test_that("Can get period date for data file",{
  period_dates <- lapply(rawDataList_conv, get_date)

  date_aug15 <- lubridate::ymd_hm('2015-08-01 00:00', tz = "Europe/London")
  date_jan18 <- lubridate::ymd_hm('2018-01-01 00:00', tz = "Europe/London")

  expect_equal(period_dates[[1]], date_aug15, info = 'August 2015 date correctly extracted')
  expect_equal(period_dates[[2]], date_jan18, info = 'January 2018 date correctly extracted')
})

test_that("Data file processed correctly into rectangualar data frames",{
  neatDataList <- lapply(rawDataList_conv, clean_AE_data)

  load('test-data/neat-colnames.rda')

  expect_equal(colnames(neatDataList[[1]]), neat_colnames,
               info = 'August 2015 column names correct post processing')
  expect_equal(colnames(neatDataList[[2]]), neat_colnames,
               info = 'January 2018 column names correct post processing')
  expect_is(neatDataList[[1]][[2]], 'character')
  expect_is(neatDataList[[2]][[2]], 'character')
  expect_match(neatDataList[[1]][[2]], 'Commissioning Region', all = TRUE,
               info = 'Second column is commissioning region - August 2015')
  expect_match(neatDataList[[2]][[2]], 'Commissioning Region', all = TRUE,
               info = 'Second column is commissioning region - January 2018')
  expect_equal(lubridate::is.POSIXct(neatDataList[[1]][[4]]), TRUE)
  expect_equal(lubridate::is.POSIXct(neatDataList[[2]][[4]]), TRUE)
  expect_is(neatDataList[[1]][[5]], 'numeric')
  expect_is(neatDataList[[2]][[5]], 'numeric')


})
