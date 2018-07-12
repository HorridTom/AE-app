library(nhsAEscraper)
context("Retrieve urls of A&E data files")


# These tests are dependent on a live internet connection
# TODO: Mock this out

test_that("getAEdata_urls_monthly returns character vector",{
  urls_ae <- getAEdata_urls_monthly()
  expect_is(urls_ae, "character")
})

test_that("getAEdata_urls_monthly returns urls with correct anatomy",{
  urls_ae <- getAEdata_urls_monthly()

  url_pattern <- '^https://www.england.nhs.uk/statistics/wp-content/uploads/sites/.*$'
  filename_pattern <- '/(?:.(?!/))+(-AE-by-provider-).+xls.?$'
  date_pattern <- '^(.*)-AE-by-provider.*$'

  url_stem_presence <- stringr::str_detect(urls_ae, url_pattern)
  filename_strings <- stringr::str_match(urls_ae, filename_pattern)[,1]
  filename_strings <- stringr::str_match(filename_strings, '^/(.*)$')[,2]
  dates <- stringr::str_match(filename_strings, date_pattern)[,2]
  dates_valid <- !is.na(zoo::as.yearmon(dates, "%B-%Y"))

  expect_equal(all(url_stem_presence), TRUE, info = 'Url stem')
  expect_equal(all(!is.na(filename_strings)), TRUE, info = 'Filename components')
  expect_equal(all(dates_valid), TRUE, info = 'Month-Year')
})

test_that("getAEdata_urls_monthly url_list argument controls which web pages to scrape",{
  url_16_17 <- paste0("https://www.england.nhs.uk/statistics/statistical-work-areas/",
                      "ae-waiting-times-and-activity/statistical-work-areasae-waiting",
                      "-times-and-activityae-attendances-and-emergency-admissions-2016-17/")

  AE_data_urls_16_17 <- getAEdata_urls_monthly(list(url_16_17))
  expect_equal(length(AE_data_urls_16_17), 12, info = '12 data file urls on 2016-2017 page')
})
