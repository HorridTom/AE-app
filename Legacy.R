get_performance <- function(df, start_date, end_date, prov_codes = c("RQM"), adm_only = FALSE, all_provs = FALSE,
                            dept_types = c('1','2','3')) {
  
  ps <- make_perf_series(df = df, prov_codes = prov_codes, adm_only = adm_only,
                         all_provs = all_provs, dept_types = dept_types)
  
  total_pts <- dplyr::filter(ps, Wk_End_Sun >= start_date, Wk_End_Sun <= end_date) %>%
    dplyr::summarize(total = sum(Total))
  met_target <- dplyr::filter(ps, Wk_End_Sun >= start_date, Wk_End_Sun <= end_date) %>%
    dplyr::summarize(total = sum(Within_4h))
  
  ((met_target/total_pts)*100)[1,1]
  
}

get_num_outside_limits <- function(df, prov_codes = c("RQM"), date.col = 'Wk_End_Sun',
                                   cht.start.date = "2014-01-01", cht.end.date = "2017-06-30",
                                   cht.brk.date = "2016-02-01",
                                   period.start = "2017-01-01", period.end = "2017-06-30",
                                   adm_only = FALSE, all_provs = FALSE,
                                   dept_types = c('1','2','3'), lower.only = TRUE) {
  
  perf_cht <- plot_performance(df = df, prov_codes = prov_codes, date.col = date.col,
                               start.date = cht.start.date, end.date = cht.end.date,
                               brk.date = cht.brk.date, adm_only = adm_only, all_provs = all_provs,
                               dept_types = dept_types, pr_name = 'Perf')
  
  pcls <- perf_cht$data$limits.signal
  pcx <- perf_cht$data$x
  pcy <- perf_cht$data$y
  lcl <- perf_cht$data$lcl
  rule1_breaks <- data.frame(pcx,pcls,pcy,lcl)
  rule1_breaks$lower.breaks <- rule1_breaks$pcy < rule1_breaks$lcl
  if(!lower.only) {
    r1binrange <- rule1_breaks %>%
      filter(pcx <= as.Date(period.end), pcx >= as.Date(period.start)) %>%
      summarise(pts_out_cl = sum(pcls))
  } else {
    r1binrange <- rule1_breaks %>%
      filter(pcx <= as.Date(period.end), pcx >= as.Date(period.start)) %>%
      summarise(pts_below_lcl = sum(lower.breaks))
  }
  
  r1binrange[1,1]
}


perf_tab <- function(org_table, perf_df, start_date, end_date, cht.start.date = "2014-01-01",
                     cht.end.date = "2017-06-30", cht.brk.date = "2016-02-01",
                     perf_col_name = "Performance", stability_col_name = "Stability",
                     lower_stab_col_name = "Lower.Stability") {
  
  pt <- org_table %>% rowwise() %>% 
    mutate(performance = get_performance(df = perf_df,
                                         start_date = start_date, end_date = end_date,
                                         prov_codes = Prov_Code)) %>%
    mutate(stability = get_num_outside_limits(df = perf_df,
                                              period.start = start_date, period.end = end_date,
                                              prov_codes = Prov_Code, lower.only = FALSE)) %>%
    mutate(lowerstability = get_num_outside_limits(df = perf_df,
                                                   period.start = start_date, period.end = end_date,
                                                   prov_codes = Prov_Code, lower.only = TRUE))
  
  pt %>% rename(!!perf_col_name := performance, !!stability_col_name := stability,
                !!lower_stab_col_name := lowerstability)
}

perf_weekly_series <- function(perf_df) {
  
  ps <- make_perf_series(df = perf_df, all_provs = TRUE, merge_provs = FALSE)
  ps %>% select(Wk_End_Sun, Prov_Code, Performance) %>% spread(key = Prov_Code, value = Performance)
  
}


plot_performance_qcc <- function(df) {
  
  library(qcc)
  
  pcc1 <- qcc(data = df[1:104,"Within_4h"], sizes = df[1:104,"Total"], plot = FALSE, type = "p", newdata = df[105:nrow(df),"Within_4h"], newsizes = df[105:nrow(df),"Total"], chart.all = TRUE, data.name = "2-year Baseline")
  pcc1$newdata.name <- "Post-baseline"
  plot(pcc1, title = "Proportion of ED attendances in department for 4h or less", xlab = "Week", ylab = "Proportion attendances", yaxt="n")
  axis(2, at=pretty(c(pcc1$statistics,pcc1$newstats)), lab = paste(pretty(c(pcc1$statistics,pcc1$newstats)) * 100, "%"), las=TRUE)
  
}


deseasonalise_performance <- function(df, prov_codes = c("RQM"), 
                                      adm_only = FALSE, all_provs = FALSE, merge_provs = NULL,
                                      dept_types = c('1','2','3'),
                                      baseline_periods = NULL) {
  
  ps <- make_perf_series(df = df, prov_codes = prov_codes, adm_only = adm_only, dept_types = dept_types)
  ps$Wk <- lubridate::week(ps$Wk_End_Sun)
  ps$Mt <- lubridate::month(ps$Wk_End_Sun)
  ps$Qt <- lubridate::quarter(ps$Wk_End_Sun)
  ps$Yr <- lubridate::year(ps$Wk_End_Sun)
  
  deseasonalise(df = ps, season_col = Mt, period_col = Yr, baseline_periods = baseline_periods)
}