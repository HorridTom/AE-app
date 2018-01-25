library(tidyverse)
library(ggplot2)
library(scales)

make_perf_series <- function(df, prov_codes = c("RBZ"), perf_only = FALSE, 
                             adm_only = FALSE, all_provs = FALSE, merge_provs = NULL,
                             dept_types = c('1','2','3'), date_col = 'Wk_End_Sun') {
  df <- df[which(df$AEA_Department_Type %in% dept_types),]
  
  # TODO: This is a horrible hack - use tidyverse throughout to avoid?
  newdatecol <- df[, date_col]
  if (class(newdatecol) == "Date") {
    df$datecol <- newdatecol
  } else {
    df$datecol <- newdatecol[[1]]
  }
  
  if(is.null(merge_provs)) {merge_provs <- all_provs}
  
  if (!all_provs) {data_prov <- df[df$Prov_Code %in% prov_codes,]} else {
    data_prov <- df
  }
  
  if (adm_only) {
    data_prov <- data_prov[data_prov$Admitted == TRUE,]
  }
  
  if (merge_provs) {
    south_4h <- aggregate(Activity ~ datecol + Greater_4h, data = data_prov, sum)
  } else {
    south_4h <- aggregate(Activity ~ datecol + Prov_Code + Greater_4h, data = data_prov, sum)
  }
  perf_4h_wide <- tidyr::spread(data = south_4h, key = Greater_4h, value = Activity)
  
  colnames(perf_4h_wide)[colnames(perf_4h_wide) == "FALSE"] <- "Within_4h"
  colnames(perf_4h_wide)[colnames(perf_4h_wide) == "TRUE"] <- "Greater_4h"

  perf_4h_wide$Total <- perf_4h_wide$Within_4h + perf_4h_wide$Greater_4h
  perf_4h_wide$Performance <- perf_4h_wide$Within_4h / perf_4h_wide$Total
  
  perf_4h_wide <- perf_4h_wide[order(perf_4h_wide$datecol),]
  
  if (perf_only) {
    perf_4h_wide <- perf_4h_wide$Performance
  } else {
    perf_4h_wide[,date_col] <- perf_4h_wide$datecol
    perf_4h_wide$datecol <- NULL
  }
  
  perf_4h_wide

}


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
                                   adm_only = FALSE,
                                   dept_types = c('1','2','3'), lower.only = TRUE) {
  
  perf_cht <- plot_performance(df = df, prov_codes = prov_codes, date.col = date.col,
                               start.date = cht.start.date, end.date = cht.end.date,
                               brk.date = cht.brk.date, adm_only = adm_only,
                               dept_types = dept_types)
  
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

plot_performance <- function(df, prov_codes = c("RBZ"), date.col = 'Wk_End_Sun',
                             start.date = "2014-01-01", end.date = "2017-06-30",
                             brk.date = "2016-02-01", max_lower_y_scale = 60,
                             adm_only = FALSE, all_provs = FALSE,
                             dept_types = c('1','2','3'), plot.chart = TRUE,
                             pr_name = NULL, x_title = "Week Ending Sunday") {
  # pass df as cleaned 4h perf data from the clean_4h_data function
  
  # if no pr_name passed, lookup full name of provider
  # note written for just one provider
  if (is_null(pr_name)) {
    pr_name <- df[which(df$Prov_Code == prov_codes),"Prov_Name"][[1]]
  }
  #cht_title = paste("Percentage ED attendances with time in department < 4h",pr_name,sep="\n")
  if (adm_only) {
    cht_title = paste("Percentage admissions through ED \n with time in department < 4h",sep="")
  } else {
    cht_title = paste("Percentage ED attendances \n with time in department < 4h",sep="")
  }
  
  df <- make_perf_series(df = df, prov_codes = prov_codes, adm_only = adm_only,
                         all_provs = all_provs, dept_types = dept_types, date_col = date.col)
  
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  br.dt <- as.Date(brk.date)
  
  # restrict to the period specified
  df <- df[df[,date.col] >= st.dt & df[,date.col] <= ed.dt,]
  
  # locate break row
  v <- df[,date.col]
  br.row <- which(v == max(v[v < br.dt]))
  
  # This is a hack - find better way to modify colours of qicharts
  # Also needs stepped limits
  
  pct <- qicharts::tcc(n = Within_4h, d = df$Total, x = df[,date.col], data = df, chart = 'p', multiply = 100, prime = TRUE, breaks = c(br.row), runvals = TRUE, cl.lab = FALSE)
  
  # chart y limit
  ylimlow <- min(min(pct$data$y, na.rm = TRUE),min(pct$data$lcl, na.rm = TRUE),max_lower_y_scale)
  
  col1    <- rgb(000, 000, 000, maxColorValue = 255)
  col2    <- rgb(241, 088, 084, maxColorValue = 255)
  col3    <- rgb(000, 000, 000, maxColorValue = 255)
  col4    <- 'white'
  col5    <-  rgb(096, 189, 104, maxColorValue = 255)
  cols    <- c('col1' = col1, 'col2' = col2, 'col3' = col3, 'col4' = col4)

  if(plot.chart == TRUE) {pct + geom_line(aes_string(x = 'x', y = 'lcl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
    geom_line(aes_string(x = 'x', y = 'ucl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
    geom_line(aes_string(x = 'x', y = 'cl', group = 'breaks'), colour = '#000000', linetype = 1) +
    geom_line(aes_string(x = 'x', y = 'y', group = 'breaks'), colour = '#000000', linetype = 1, lwd = 1.1) + 
    geom_point(aes_string(x = 'x', y = 'y', group = 'breaks', fill = 'pcol'), size = 2) + 
    scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
    labs(title = cht_title, x= x_title, y="Percentage", subtitle = pr_name) +
    ylim(ylimlow,100) + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("3 months"), limits = as.Date(c(start.date, end.date))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
          axis.text.y = element_text(size = 14), axis.title = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.subtitle = element_text(size = 16, face = "italic"),
          axis.line = element_line(colour = "grey60"))
  } else {df}
    
}

plot_volume <- function(df, prov_codes = c("RBZ"), date.col = 'Wk_End_Sun',
                        start.date = "2014-01-01", end.date = "2017-02-28",
                        brk.date = "2016-01-01", min_upper_y_scale = 3000,
                        adm_only = FALSE, all_provs = FALSE,
                        dept_types = c('1','2','3'), x_title = "Week Ending Sunday") {
  # pass df as cleaned 4h perf data from the clean_4h_data function
  
  
  # lookup full name of provider
  # note written for just one provider
  pr_name <- df[which(df$Prov_Code == prov_codes),"Prov_Name"][[1]]
  #cht_title = paste("Weekly total ED attendances",pr_name,sep="\n")
  if (adm_only) {
    cht_title = paste("Weekly total admissions through ED. Dept. types: ",paste(dept_types,sep="",collapse=","),sep="")
    y_axis_lab = "Admissions"
  } else {
    cht_title = paste("Weekly total ED attendances. Dept. types: ",paste(dept_types,sep="",collapse=","),sep="")
    y_axis_lab = "Attendances"
  }
  
  df <- make_perf_series(df = df, prov_codes = prov_codes, adm_only = adm_only, all_provs = all_provs, dept_types = dept_types, date_col = date.col)
  
  df$datecol <- df[, date.col]
  
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  br.dt <- as.Date(brk.date)
  
  # restrict to the period specified
  df <- df[df[,date.col] >= st.dt & df[,date.col] <= ed.dt,]
  
  # chart y limit
  ylimhigh <- max(max(df$Total),min_upper_y_scale)
  
  pp <- ggplot(data = df, aes(x = datecol, y = Total))
  
  pp + geom_path() + geom_point() + ylim(0,ylimhigh) +
  scale_x_date(labels = date_format("%Y-%m"),breaks = date_breaks("3 months")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line=element_line(colour = "grey75"), axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.75), plot.title = element_text(hjust = 0.5)) +
  labs(title = cht_title, x=x_title, y=y_axis_lab) +
  geom_vline(xintercept = as.numeric(br.dt), colour="grey60")
  
}


plot_volume_c <- function(df, prov_codes = c("RBZ"), date.col = 'Wk_End_Sun',
                             start.date = "2014-01-01", end.date = "2017-02-28",
                             brk.date = "2016-01-01", min_upper_y_scale = 3000,
                             adm_only = FALSE, all_provs = FALSE,
                             dept_types = c('1','2','3'), plot.chart = TRUE,
                             pr_name = NULL, x_title = "Week Ending Sunday") {
  # pass df as cleaned 4h perf data from the clean_4h_data function
  
  # if no pr_name passed, lookup full name of provider
  # note written for just one provider
  if (is_null(pr_name)) {
    pr_name <- df[which(df$Prov_Code == prov_codes),"Prov_Name"][[1]]
  }
  #cht_title = paste("Weekly percentage ED attendances with time in department < 4h",pr_name,sep="\n")
  if (adm_only) {
    cht_title = paste("Weekly total admissions through ED. Dept. types: ",paste(dept_types,sep="",collapse=","),sep="")
    y_axis_lab = "Admissions"
  } else {
    cht_title = paste("Weekly total ED attendances. Dept. types: ",paste(dept_types,sep="",collapse=","),sep="")
    y_axis_lab = "Attendances"
  }
  
  df <- make_perf_series(df = df, prov_codes = prov_codes, adm_only = adm_only,
                         all_provs = all_provs, dept_types = dept_types, date_col = date.col)
  
  df$datecol <- df[, date.col]
  
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  br.dt <- as.Date(brk.date)
  
  # restrict to the period specified
  df <- df[df[,date.col] >= st.dt & df[,date.col] <= ed.dt,]
  
  # locate break row
  v <- df[,date.col]
  br.row <- which(v == max(v[v < br.dt]))
  
  # This is a hack - find better way to modify colours of qicharts
  # Also needs stepped limits
  
  pct <- qicharts::tcc(n = Total, d = rep(1, nrow(df)), x = df$datecol, data = df, chart = 'u', prime = TRUE, breaks = c(br.row), runvals = TRUE, cl.lab = TRUE)
  
  # chart y limit
  ylimhigh <- max(max(df$Total),min_upper_y_scale)
  
  col1    <- rgb(000, 000, 000, maxColorValue = 255)
  col2    <- rgb(241, 088, 084, maxColorValue = 255)
  col3    <- rgb(000, 000, 000, maxColorValue = 255)
  col4    <- 'white'
  col5    <-  rgb(096, 189, 104, maxColorValue = 255)
  cols    <- c('col1' = col1, 'col2' = col2, 'col3' = col3, 'col4' = col4)
  
  if(plot.chart == TRUE) {pct + geom_line(aes_string(x = 'x', y = 'lcl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
      geom_line(aes_string(x = 'x', y = 'ucl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
      geom_line(aes_string(x = 'x', y = 'cl', group = 'breaks'), colour = '#000000', linetype = 1) +
      geom_line(aes_string(x = 'x', y = 'y', group = 'breaks'), colour = '#000000', linetype = 1, lwd = 1.1) + 
      geom_point(aes_string(x = 'x', y = 'y', group = 'breaks', fill = 'pcol'), size = 2) + 
      scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
      labs(title = cht_title, x=x_title, y=y_axis_lab, subtitle = pr_name) +
      ylim(0,ylimhigh) + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("3 months"), limits = as.Date(c(start.date, end.date))) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.75), plot.title = element_text(hjust = 0.5), axis.line = element_line(colour = "grey60"))
  } else {df}
  
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
