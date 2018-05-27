source('../perf_4h_analysis.R')
context("Make a performance timeseries dataset")

load('test-data/AE_data_test.rda')
load('test-data/perf_data_colnames.rda')

#p4h <- make_p4h_from_sitreps(AE_data_test)

testthat::test_that("Created timeseries dataset is a dataframe",{
  ps_rqm_all_all <- make_perf_series(df = AE_data_test, prov_codes = c('RQM'),
                                          measure = 'All')
  ps_rqm_all_typ1 <- make_perf_series(df = AE_data_test, prov_codes = c('RQM'),
                                     measure = 'Typ1')
  ps_rqm_adm_all <- make_perf_series(df = AE_data_test, prov_codes = c('RQM'),
                                     measure = 'Adm')
  
  testthat::expect_is(ps_rqm_all_all, 'data.frame')
  testthat::expect_is(ps_rqm_all_typ1, 'data.frame')
  testthat::expect_is(ps_rqm_adm_all, 'data.frame')
})

testthat::test_that("Results for selected months in performance series for Chelsea and Westminster are correct",{
  ps_rqm_all_all <- make_perf_series(df = AE_data_test, prov_codes = c('RQM'),
                                            measure = 'All')
  ps_rqm_all_typ1 <- make_perf_series(df = AE_data_test, prov_codes = c('RQM'),
                                             measure = 'Typ1')
  ps_rqm_adm_all <- make_perf_series(df = AE_data_test, prov_codes = c('RQM'),
                                            measure = 'Adm')
  
  # Test that we get the correct results for Chelsea and Westminster (RQM)
  # in April 2017, for all attendances
  testthat::expect_equal(ps_rqm_all_all %>%
                           filter(Month_Start == "2017-04-01") %>%
                           top_n(1) %>% pull(Total),
                         23452)
  testthat::expect_equal(ps_rqm_all_all %>%
                           filter(Month_Start == "2017-04-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         22059)
  testthat::expect_equal(ps_rqm_all_all %>%
                           filter(Month_Start == "2017-04-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         1393)
  # Test that we get the correct results for Chelsea and Westminster (RQM)
  # in April 2017, for type 1 attendances
  testthat::expect_equal(ps_rqm_all_typ1 %>%
                           filter(Month_Start == "2017-04-01") %>%
                           top_n(1) %>% pull(Total),
                         15957)
  testthat::expect_equal(ps_rqm_all_typ1 %>%
                           filter(Month_Start == "2017-04-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         14648)
  testthat::expect_equal(ps_rqm_all_typ1 %>%
                           filter(Month_Start == "2017-04-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         1309)
  # Test that we get the correct results for Chelsea and Westminster (RQM)
  # in April 2017, for admissions only
  testthat::expect_equal(ps_rqm_adm_all %>%
                           filter(Month_Start == "2017-04-01") %>%
                           top_n(1) %>% pull(Total),
                         3375)
  testthat::expect_equal(ps_rqm_adm_all %>%
                           filter(Month_Start == "2017-04-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         3194)
  testthat::expect_equal(ps_rqm_adm_all %>%
                           filter(Month_Start == "2017-04-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         181)
})

testthat::test_that("Results for selected months in performance series for Imperial are correct",{
  ps_ryj_all_all <- make_perf_series(df = AE_data_test, prov_codes = c('RYJ'),
                                     measure = 'All')
  ps_ryj_all_typ1 <- make_perf_series(df = AE_data_test, prov_codes = c('RYJ'),
                                      measure = 'Typ1')
  ps_ryj_adm_all <- make_perf_series(df = AE_data_test, prov_codes = c('RYJ'),
                                     measure = 'Adm')
  
  # Test that we get the correct results for Imperial (RYJ)
  # in September 2017, for all attendances
  testthat::expect_equal(ps_ryj_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Total),
                         23637)
  testthat::expect_equal(ps_ryj_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         20577)
  testthat::expect_equal(ps_ryj_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         3060)
  # Test that we get the correct results for Imperial (RYJ)
  # in September 2017, for type 1 attendances
  testthat::expect_equal(ps_ryj_all_typ1 %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Total),
                         9582)
  testthat::expect_equal(ps_ryj_all_typ1 %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         6713)
  testthat::expect_equal(ps_ryj_all_typ1 %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         2869)
  # Test that we get the correct results for Imperial (RYJ)
  # in September 2017, for admissions only
  testthat::expect_equal(ps_ryj_adm_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Total),
                         3354)
  testthat::expect_equal(ps_ryj_adm_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         2752)
  testthat::expect_equal(ps_ryj_adm_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         602)
})