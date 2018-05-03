source('../perf_4h_analysis.R')
context("Convert downloaded sitrep data to tidy data")

load('test-data/AE_data_test.rda')

testthat::test_that("Output of conversion is a tibble",{
    p4h <- make_p4h_from_sitreps(AE_data_test)
    testthat::expect_is(p4h, 'tbl')
})
