source('../perf_4h_analysis.R')
context("Make a performance timeseries dataset")

load('test-data/AE_data_test.rda')
load('test-data/perf_data_colnames.rda')


testthat::test_that("Created timeseries dataset is a dataframe",{
  p4h <- make_p4h_from_sitreps(AE_data_test)
  ps_rqm_all_all <- make_perf_series(df = p4h_test, prov_codes = c('RQM'),
                                          adm_only = FALSE, dept_types = c(1,2,3),
                                          date_col = 'Month_Start')
  ps_rqm_all_typ1 <- make_perf_series(df = p4h_test, prov_codes = c('RQM'),
                                     adm_only = FALSE, dept_types = c(1),
                                     date_col = 'Month_Start')
  ps_rqm_adm_all <- make_perf_series(df = p4h_test, prov_codes = c('RQM'),
                                     adm_only = TRUE, dept_types = NA,
                                     date_col = 'Month_Start')
  
  testthat::expect_is(ps_rqm_all_all, 'data.frame')
  testthat::expect_is(ps_rqm_all_typ1, 'data.frame')
  testthat::expect_is(ps_rqm_adm_all, 'data.frame')
})