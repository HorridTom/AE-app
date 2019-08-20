context("Make a performance timeseries dataset")
source('../perf_4h_analysis.R')

load('test-data/AE_data_test.rda')
load('test-data/perf_data_colnames.rda')

#p4h <- make_p4h_from_sitreps(AE_data_test)

testthat::test_that("Created timeseries dataset is a dataframe",{
  ps_rqm_all_all <- make_perf_series(df = AE_data_test, code = 'RQM',
                                          measure = 'All', level = "Provider", 
                                          weeklyOrMonthly = "monthly",
                                          onlyProvsReporting = F)
  ps_rqm_all_typ1 <- make_perf_series(df = AE_data_test, code = 'RQM',
                                     measure = 'Typ1', level = "Provider", 
                                     weeklyOrMonthly = "monthly",
                                     onlyProvsReporting = F)
  ps_rqm_adm_all <- make_perf_series(df = AE_data_test, code = 'RQM',
                                     measure = 'Adm', level = "Provider", 
                                     weeklyOrMonthly = "monthly",
                                     onlyProvsReporting = F)
  
  testthat::expect_is(ps_rqm_all_all, 'data.frame')
  testthat::expect_is(ps_rqm_all_typ1, 'data.frame')
  testthat::expect_is(ps_rqm_adm_all, 'data.frame')
})

testthat::test_that("Results for selected months in performance series for Chelsea and Westminster are correct",{
  ps_rqm_all_all <- make_perf_series(df = AE_data_test, code = 'RQM',
                                            measure = 'All', level = "Provider", 
                                            weeklyOrMonthly = "monthly",
                                            onlyProvsReporting = F)
  ps_rqm_all_typ1 <- make_perf_series(df = AE_data_test, code = 'RQM',
                                             measure = 'Typ1', level = "Provider", 
                                             weeklyOrMonthly = "monthly",
                                             onlyProvsReporting = F)
  ps_rqm_adm_all <- make_perf_series(df = AE_data_test, code = 'RQM',
                                            measure = 'Adm', level = "Provider", 
                                            weeklyOrMonthly = "monthly",
                                            onlyProvsReporting = F)
  
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
  ps_ryj_all_all <- make_perf_series(df = AE_data_test, code = 'RYJ',
                                     measure = 'All', level = "Provider", 
                                     weeklyOrMonthly = "monthly",
                                     onlyProvsReporting = F)
  ps_ryj_all_typ1 <- make_perf_series(df = AE_data_test, code = 'RYJ',
                                      measure = 'Typ1', level = "Provider", 
                                      weeklyOrMonthly = "monthly",
                                      onlyProvsReporting = F)
  ps_ryj_adm_all <- make_perf_series(df = AE_data_test, code = 'RYJ',
                                     measure = 'Adm', level = "Provider", 
                                     weeklyOrMonthly = "monthly",
                                     onlyProvsReporting = F)
  
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


testthat::test_that("Results for selected months in performance series for London region are correct",{
  ps_lo_all_all <- make_perf_series(df = AE_data_test, code = 'Lo',
                                     measure = 'All', level = "Regional", 
                                     weeklyOrMonthly = "monthly",
                                     onlyProvsReporting = F)
  
  # Test that we get the correct results for London region (Lo)
  # in September 2017, for all attendances
  testthat::expect_equal(ps_lo_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Total),
                         395373)
  testthat::expect_equal(ps_lo_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         356603)
  testthat::expect_equal(ps_lo_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         38770)

})

testthat::test_that("Results for selected months in performance series for whole of England are correct",{
  ps_e_all_all <- make_perf_series(df = AE_data_test, code = 'E',
                                    measure = 'All', level = "National", 
                                    weeklyOrMonthly = "monthly",
                                    onlyProvsReporting = F)
  
  # Test that we get the correct results for England (E)
  # in September 2017, for all attendances
  testthat::expect_equal(ps_e_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Total),
                         1925961)
  testthat::expect_equal(ps_e_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         1726934)
  testthat::expect_equal(ps_e_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         199027)
  
})

###################
#tests for Scotland
#tolerances included due to the week to month conversion 
testthat::test_that("Results for selected months in performance series for University Hospital Ayr are correct",{
  ps_A210H_all_all <- make_perf_series(df = AE_data_test, code = 'A210H',
                                     measure = 'All', level = "Provider", 
                                     weeklyOrMonthly = "monthly",
                                     onlyProvsReporting = F)
  
  # Test that we get the correct results for University Hospital Ayr (A210H)
  # in September 2017, for all attendances
  testthat::expect_equal(ps_A210H_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Total),
                         3156, tolerance = 2)
  testthat::expect_equal(ps_A210H_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         2889, tolerance = 2)
  testthat::expect_equal(ps_A210H_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         267, tolerance = 2)
})

testthat::test_that("Results for selected months in performance series for Greater Glasgow & Clyde board are correct",{
  ps_g_all_all <- make_perf_series(df = AE_data_test, code = 'G',
                                       measure = 'All', level = "Regional", 
                                       weeklyOrMonthly = "monthly",
                                       onlyProvsReporting = F)
  
  # Test that we get the correct results for Greater Glasgow & Clyde board (G)
  # in september 2017, for all attendances
  testthat::expect_equal(ps_g_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Total),
                         30070, tolerance = 2)
  testthat::expect_equal(ps_g_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         27042, tolerance = 2)
  testthat::expect_equal(ps_g_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         3028, tolerance = 2)
})

testthat::test_that("Results for selected months in performance series for whole of Scotland are correct",{
  ps_s_all_all <- make_perf_series(df = AE_data_test, code = 'S',
                                   measure = 'All', level = "National", 
                                   weeklyOrMonthly = "monthly",
                                   onlyProvsReporting = F)
  
  # Test that we get the correct results for whole of Scotland (S)
  # in sept 2017, for all attendances
  testthat::expect_equal(ps_s_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Total),
                         115617, tolerance = 10)
  testthat::expect_equal(ps_s_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         106880, tolerance = 10)
  testthat::expect_equal(ps_s_all_all %>%
                           filter(Month_Start == "2017-09-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         8737, tolerance = 10)
})

#####tests for weekly data
testthat::test_that("Results for selected weeks in performance series for whole of Scotland are correct for weekly data",{
  ps_s_all_all_weekly <- make_perf_series(df = AE_data_test, code = 'S',
                                   measure = 'All', level = "National", 
                                   weeklyOrMonthly = "weekly",
                                   onlyProvsReporting = F)
  
  # Test that we get the correct results for whole of Scotland (S)
  # in jan 2017, for all attendances by week
  testthat::expect_equal(ps_s_all_all_weekly %>%
                           filter(Month_Start == "2017-01-01") %>%
                           top_n(1) %>% pull(Total),
                         25050, tolerance = 10)
  testthat::expect_equal(ps_s_all_all_weekly %>%
                           filter(Month_Start == "2017-01-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         23179, tolerance = 10)
  testthat::expect_equal(ps_s_all_all_weekly %>%
                           filter(Month_Start == "2017-01-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         1873, tolerance = 10)
})

testthat::test_that("Results for selected weeks in performance series for Greater Glasgow & Clyde board (G) are correct for weekly data",{
  ps_g_all_all_weekly <- make_perf_series(df = AE_data_test, code = 'G',
                                          measure = 'All', level = "Regional", 
                                          weeklyOrMonthly = "weekly",
                                          onlyProvsReporting = F)
  
  # Test that we get the correct results for Greater Glasgow & Clyde board (G)
  # in jan 2017, for all attendances by week
  testthat::expect_equal(ps_g_all_all_weekly %>%
                           filter(Month_Start == "2017-01-01") %>%
                           top_n(1) %>% pull(Total),
                         6488, tolerance = 10)
  testthat::expect_equal(ps_g_all_all_weekly %>%
                           filter(Month_Start == "2017-01-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         5677, tolerance = 10)
  testthat::expect_equal(ps_g_all_all_weekly %>%
                           filter(Month_Start == "2017-01-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         811, tolerance = 10)
})

testthat::test_that("Results for selected weeks in performance series for Greater Glasgow & Clyde board (G) are correct for weekly data",{
  ps_A210H_all_all_weekly <- make_perf_series(df = AE_data_test, code = 'A210H',
                                          measure = 'All', level = "Provider", 
                                          weeklyOrMonthly = "weekly",
                                          onlyProvsReporting = F)
  
  #Test that we get the correct results for University Hospital Ayr (A210H)
  # in jan 2017, for all attendances by week
  testthat::expect_equal(ps_A210H_all_all_weekly %>%
                           filter(Month_Start == "2017-01-01") %>%
                           top_n(1) %>% pull(Total),
                         881, tolerance = 10)
  testthat::expect_equal(ps_A210H_all_all_weekly %>%
                           filter(Month_Start == "2017-01-01") %>%
                           top_n(1) %>% pull(Within_4h),
                         861, tolerance = 10)
  testthat::expect_equal(ps_A210H_all_all_weekly %>%
                           filter(Month_Start == "2017-01-01") %>%
                           top_n(1) %>% pull(Greater_4h),
                         20, tolerance = 10)
})