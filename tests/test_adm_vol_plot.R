context("Plot admissions volume")
source('../perf_4h_analysis.R')
source("../spc_rules.R")

load('test-data/AE_data_test.rda')

AE_data_test <- clean_region_col(AE_data_test)

testthat::test_that("Admissions volume plot is correct for April 2017 for Chelsea and Westminster",{
vchart_adm_all_ed <- plot_volume(df = AE_data_test, code = 'RQM', brk.date = NULL, measure = 'Adm_All_ED',
                           start.date = "2014-01-01", end.date = "2017-06-30",
                           weeklyOrMonthly = "monthly")
vchart_adm_typ1 <- plot_volume(df = AE_data_test, code = 'RQM', brk.date = NULL, measure = 'Adm_Typ1',
                           start.date = "2014-01-01", end.date = "2017-06-30",
                           weeklyOrMonthly = "monthly")
vchart_adm_all <- plot_volume(df = AE_data_test, code = 'RQM', brk.date = NULL, measure = 'Adm_All',
                           start.date = "2014-01-01", end.date = "2017-06-30",
                           weeklyOrMonthly = "monthly")


# Check output is a ggplot
testthat::expect_is(vchart_adm_all_ed, "ggplot")
testthat::expect_is(vchart_adm_typ1, "ggplot")
testthat::expect_is(vchart_adm_all, "ggplot")

#check correct chart title for ggplot
testthat::expect_match(vchart_adm_all_ed$labels$title, "admissions")
testthat::expect_match(vchart_adm_all_ed$labels$title, "month")
testthat::expect_match(vchart_adm_typ1$labels$title,"admissions")
testthat::expect_match(vchart_adm_typ1$labels$title,"month")
testthat::expect_match(vchart_adm_all$labels$title,"admissions")
testthat::expect_match(vchart_adm_all$labels$title,"month")



#check correct y-axis label for ggplot
testthat::expect_match(vchart_adm_all_ed$labels$y,"admissions")
testthat::expect_match(vchart_adm_typ1$labels$y, "admissions")
testthat::expect_match(vchart_adm_all$labels$y,"admissions")

#check correct subtitle label for ggplot
testthat::expect_match(vchart_adm_all_ed$labels$subtitle, "Chelsea")
testthat::expect_match(vchart_adm_all_ed$labels$subtitle, "All")
testthat::expect_match(vchart_adm_typ1$labels$subtitle,"Chelsea")
testthat::expect_match(vchart_adm_typ1$labels$subtitle,"Type 1")
testthat::expect_match(vchart_adm_all$labels$subtitle, "Chelsea")
testthat::expect_match(vchart_adm_all$labels$subtitle, "All")



# Check exactly one x value for April 2017
Apr2017 <- as.Date("2017-04-01", tz = "Europe/London")
testthat::expect_equal(sum(vchart_adm_all_ed$data$x == Apr2017), 1)
testthat::expect_equal(sum(vchart_adm_typ1$data$x == Apr2017), 1)
testthat::expect_equal(sum(vchart_adm_all$data$x == Apr2017), 1)


# Check correct volume for April 2017
testthat::expect_equal(vchart_adm_all_ed$data$y[which(vchart_adm_all_ed$data$x == Apr2017)],
                       3375, tolerance = .001, scale = 1)
testthat::expect_equal(vchart_adm_typ1$data$y[which(vchart_adm_typ1$data$x == Apr2017)],
                       3375, tolerance = .001, scale = 1)
testthat::expect_equal(vchart_adm_all$data$y[which(vchart_adm_all$data$x == Apr2017)],
                       4058, tolerance = .001, scale = 1)



# Check correct centre line at April 2017
testthat::expect_equal(vchart_adm_all_ed$data$cl[which(vchart_adm_all_ed$data$x == Apr2017)],
                       3477.667, tolerance = .001, scale = 1)
testthat::expect_equal(vchart_adm_typ1$data$cl[which(vchart_adm_typ1$data$x == Apr2017)],
                       3477.667, tolerance = .001, scale = 1)
testthat::expect_equal(vchart_adm_all$data$cl[which(vchart_adm_all$data$x == Apr2017)],
                       4130.5, tolerance = .001, scale = 1)



# Check correct upper control limit at April 2017
testthat::expect_equal(vchart_adm_all_ed$data$ucl[which(vchart_adm_all_ed$data$x == Apr2017)],
                       4283.518, tolerance = .001, scale = 1)
testthat::expect_equal(vchart_adm_typ1$data$ucl[which(vchart_adm_typ1$data$x == Apr2017)],
                       4283.518, tolerance = .001, scale = 1)
testthat::expect_equal(vchart_adm_all$data$ucl[which(vchart_adm_all$data$x == Apr2017)],
                       5137.415, tolerance = .001, scale = 1)


# Check correct lower control limit at April 2017
testthat::expect_equal(vchart_adm_all_ed$data$lcl[which(vchart_adm_all_ed$data$x == Apr2017)],
                       2671.816, tolerance = .001, scale = 1)
testthat::expect_equal(vchart_adm_typ1$data$lcl[which(vchart_adm_typ1$data$x == Apr2017)],
                       2671.816, tolerance = .001, scale = 1)
testthat::expect_equal(vchart_adm_all$data$lcl[which(vchart_adm_all$data$x == Apr2017)],
                       3123.585, tolerance = .001, scale = 1)


})
