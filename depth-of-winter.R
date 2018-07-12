library(tidyverse)
library(stringr)
library(lubridate)

library(nhsAEscraper)

Sys.setenv(TZ='Europe/London')
source("perf_4h_analysis.R")

make_dow_dataset <- function(df, start.date = "2015-07-01", end.date = "2018-05-30",
                             brk.date = NULL, measure = "All") {
  # Make a list of all the provider codes in the dataset df
  provLookup <- df[!duplicated(df[,c('Prov_Code')]),c('Prov_Code','Prov_Name')] %>%
    arrange(Prov_Name)
  
  # Loop around provider codes, performing the 'depth of winter' analysis for each one
  dow <- lapply(provLookup$Prov_Code, function(x) {
    get_dow_data(df = AE_Data, prov_code = x, start.date = start.date, end.date = end.date,
                 brk.date = brk.date, measure = measure)
  })
  
  # Stitch the resulting list together into a dataframe
  dow_df <- do.call(rbind, dow)
  
  # Make additional columns
  dow_df %>% mutate(winter_run_depth_1617 = Winter_min_1617 - Average,
                    winter_run_width_1617 = months_between(Winter_run_start_1617, Winter_run_end_1617) + 1,
                    winter_sc_width_1617 = months_between(Winter_sc_start_1617, Winter_sc_end_1617) + 1,
                    winter_run_depth_1718 = Winter_min_1718 - Average,
                    winter_run_width_1718 = months_between(Winter_run_start_1718, Winter_run_end_1718) + 1,
                    winter_sc_width_1718 = months_between(Winter_sc_start_1718, Winter_sc_end_1718) + 1)
  
}

get_dow_data <- function(df, prov_code = 'RQM', start.date = "2015-07-01", end.date = "2018-05-30",
                     brk.date = NULL, measure = "All") {
  plot_data <- plot_performance(df = df, prov_codes = prov_code, start.date = start.date,
                                end.date = end.date, brk.date = brk.date, measure = measure,
                                plot.chart = FALSE) %>% arrange(x)
  pr_name <- df %>% filter(Prov_Code == prov_code) %>% top_n(1, wt = Month_Start) %>% pull(Prov_Name)
  cl <- plot_data %>% top_n(1, wt = x) %>% pull(cl)
  lcl <- plot_data %>% top_n(1, wt = x) %>% pull(lcl)
  ucl <- plot_data %>% top_n(1, wt = x) %>% pull(ucl)
  average_att <- plot_data %>% pull(n) %>% mean()
  min_6months_1617 <- plot_data %>% filter(x >= as.Date("2016-10-01", tz='Europe/London'), 
                                           x <= as.Date("2017-03-01", tz='Europe/London')) %>%
                                    pull(y) %>% min()
  min_month_1617 <- plot_data %>% filter(x >= as.Date("2016-10-01", tz='Europe/London'), 
                                         x <= as.Date("2017-03-01", tz='Europe/London')) %>%
                                  slice(which.min(y)) %>% pull(x)
  if(!is.finite(min_6months_1617)) {
    min_6months_1617 <- NA
    min_month_1617 <- NA
  }
  min_below_cl_1617 <- min_6months_1617 < cl
  min_below_lcl_1617 <- min_6months_1617 < lcl
  dur_data1617 <- get_durations(plot_data, min_month = min_month_1617)
  
  min_6months_1718 <- plot_data %>% filter(x >= as.Date("2017-10-01", tz='Europe/London'), 
                                           x <= as.Date("2018-03-01", tz='Europe/London')) %>%
    pull(y) %>% min()
  min_month_1718 <- plot_data %>% filter(x >= as.Date("2017-10-01", tz='Europe/London'), 
                                         x <= as.Date("2018-03-01", tz='Europe/London')) %>%
                                  slice(which.min(y)) %>% pull(x)
  if(!is.finite(min_6months_1718)) {
    min_6months_1718 <- NA
    min_month_1718 <- NA
  }
  min_below_cl_1718 <- min_6months_1718 < cl
  min_below_lcl_1718 <- min_6months_1718 < lcl
  dur_data1718 <- get_durations(plot_data, min_month = min_month_1718)
  
  as.data.frame(list(Prov_Name=pr_name, Prov_Code=prov_code, Average=cl, LCL=lcl, UCL=ucl, Average_att=average_att,
                     Winter_min_1617=min_6months_1617, Winter_min_month_1617=min_month_1617,
                     Min_below_av_1617=min_below_cl_1617, Min_below_lcl_1617=min_below_lcl_1617,
                     Winter_run_start_1617=dur_data1617[[2]], Winter_run_end_1617=dur_data1617[[3]],
                     Winter_sc_start_1617=dur_data1617[[4]], Winter_sc_end_1617=dur_data1617[[5]],
                     Winter_min_1718=min_6months_1718, Winter_min_month_1718=min_month_1718,
                     Min_below_av_1718=min_below_cl_1718, Min_below_lcl_1718=min_below_lcl_1718,
                     Winter_run_start_1718=dur_data1718[[2]], Winter_run_end_1718=dur_data1718[[3]],
                     Winter_sc_start_1718=dur_data1718[[4]], Winter_sc_end_1718=dur_data1718[[5]]))
}


get_durations <- function(df, min_month) {
  runs <- rle(ifelse(df$y > df$cl,1,-1))
  below_lcl <- rle(ifelse(df$y >= df$lcl,1,-1))
  df$rule2 <- inverse.rle(runs)
  df$belowlim <- inverse.rle(below_lcl)
  
  csrl <- cumsum(runs$lengths)
  min_row <- which(df$x == min_month)
  run_no <- min(which(csrl>=min_row))
  winter_run_start <- as.Date(ifelse(df$rule2[min_row]==-1,df[csrl[run_no - 1]+1,] %>% pull(x), NA))
  winter_run_end <- as.Date(ifelse(df$rule2[min_row]==-1,df[csrl[run_no],] %>% pull(x), NA))
  
  csrl_l <- cumsum(below_lcl$lengths)
  run_no_l <- min(which(csrl_l>=min_row))
  winter_sc_start <- as.Date(ifelse(df$belowlim[min_row]==-1,df[csrl_l[run_no_l - 1]+1,] %>% pull(x), NA))
  winter_sc_end <- as.Date(ifelse(df$belowlim[min_row]==-1,df[csrl_l[run_no_l],] %>% pull(x), NA))
  if(length(winter_run_start) == 0) winter_run_start <- as.Date(NA)
  if(length(winter_run_end) == 0) winter_run_end <- as.Date(NA)
  if(length(winter_sc_start) == 0) winter_sc_start <- as.Date(NA)
  if(length(winter_sc_end) == 0) winter_sc_end <- as.Date(NA)
  
  list(df, winter_run_start, winter_run_end, winter_sc_start, winter_sc_end)
}

months_between <- function(d1, d2) {
  x=lubridate::interval(ymd(d1),ymd(d2))
  x %/% months(1)
}
