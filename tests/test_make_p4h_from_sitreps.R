source('../perf_4h_analysis.R')
context("Convert downloaded sitrep data to tidy data")

load('test-data/AE_data_test.rda')
load('test-data/perf_data_colnames.rda')

testthat::test_that("Output of conversion is a tibble",{
    p4h <- make_p4h_from_sitreps(AE_data_test)
    
    testthat::expect_is(p4h, 'tbl')
})

testthat::test_that("Output of conversion has correct dimensions and colnames",{
    p4h <- make_p4h_from_sitreps(AE_data_test)

    testthat::expect_equal(colnames(p4h), perf_data_colnames)
    testthat::expect_equal(nrow(p4h), 192)
})

# Should rapidly refactor to remove need to go to this data format if possible.
#testthat::test_that("Some example org-months are correctly converted",{
#    p4h <- make_p4h_from_sitreps(AE_data_test)
#})