library(nhsAEscraper)
context("Retrieve and assemble the A&E data")


test_that("",{
  AE_data <- getAE_data(update_data = FALSE, directory = 'test-data/')

  expect_is(AE_data, 'data.frame')
  expect_equal(ncol(AE_data), 22)
  expect_equal(any(AE_data$Month_Start == lubridate::ymd_hm('2015-08-01 00:00', tz = 'Europe/London')), TRUE)
  expect_equal(any(AE_data$Month_Start == lubridate::ymd_hm('2018-01-01 00:00', tz = 'Europe/London')), TRUE)

})
