library(tidyverse)
library(dplyr)
library(qicharts2)
library(ggplot2)
library(scales)
library(zoo)

make_perf_series <- function(df, prov_codes = c("RQM"), measure = "All") {
  
  df <- df %>% filter(Prov_Code %in% prov_codes)
  
  # Make new variables
  df <- df %>% mutate(Att_Typ1_NotBr = Att_Typ1 - Att_Typ1_Br,
                      Att_Typ2_NotBr = Att_Typ2 - Att_Typ2_Br,
                      Att_Typ3_NotBr = Att_Typ3 - Att_Typ3_Br,
                      Att_All_NotBr = Att_All - Att_All_Br,
                      E_Adm_Not4hBr_D = E_Adm_All_ED - E_Adm_4hBr_D)
  
  perf_series <- switch(measure,
         All = df %>%
           select(Prov_Code, Month_Start, Region, Prov_Name,
                  Within_4h = Att_All_NotBr, Greater_4h = Att_All_Br, Total = Att_All),
         Typ1 = df %>%
           select(Prov_Code, Month_Start, Region, Prov_Name,
                  Within_4h = Att_Typ1_NotBr, Greater_4h = Att_Typ1_Br, Total = Att_Typ1),
         Adm = df %>%
           select(Prov_Code, Month_Start, Region, Prov_Name,
                  Within_4h = E_Adm_Not4hBr_D, Greater_4h = E_Adm_4hBr_D, Total = E_Adm_All_ED)
           )
  perf_series %>% mutate(Performance = Within_4h / Total) %>%
    mutate(Month_Start = as.Date(Month_Start, tz = 'Europe/London')) %>%
    arrange(Month_Start)
}


plot_performance <- function(df, prov_codes = c("RBZ"), date.col = 'Month_Start',
                             start.date = "2015-07-01", end.date = "2018-05-30",
                             brk.date = NULL, max_lower_y_scale = 60,
                             measure = "All", plot.chart = TRUE,
                             pr_name = NULL, x_title = "Month") {

  cht_title = "Percentage A&E attendances\nwith time in department < 4h"
  
  df <- make_perf_series(df = df, prov_codes = prov_codes, measure = measure)
  
  # if no pr_name passed, lookup full name of provider
  if (is.null(pr_name)) {pr_name <- df %>% top_n(1, wt = Performance) %>% pull(Prov_Name)}
  
  # convert arguments to dates
  st.dt <- as.Date(start.date, tz = "Europe/London")
  ed.dt <- as.Date(end.date, tz = "Europe/London")
  q.st.dt <- as.Date(zoo::as.yearqtr(st.dt, format="%Y-%m-%d"))
  q.ed.dt <- as.Date(zoo::as.yearqtr(ed.dt, format="%Y-%m-%d"), frac = 1) + 1
  cht_axis_breaks <- seq(q.st.dt, q.ed.dt, "quarters")
  
  # restrict to the period specified
  df <- df %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  if(nrow(df)==0) {stop("No data for provider period specified")}

  if (is.null(brk.date)) {
    pct <- qicharts2::qic(Month_Start, Within_4h, n = Total, data = df, chart = 'pp', multiply = 100)
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    pct <- ggplot(pct$data, aes(x,y, label = x))
  } else {
    br.dt <- as.Date(brk.date)
    # locate break row
    v <- df$Month_Start
    br.row <- which(v == max(v[v < br.dt]))
    
    pct <- qicharts2::qic(Month_Start, Within_4h, n = Total, data = df, chart = 'pp', multiply = 100,
                          freeze = br.row)
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    pct <- ggplot(pct$data, aes(x,y))
  }
  
  # chart y limit
  ylimlow <- min(min(pct$data$y, na.rm = TRUE),min(pct$data$lcl, na.rm = TRUE), max_lower_y_scale)
  # data for target line
  cutoff <- data.frame(yintercept=95, cutoff=factor(95))
  
  if(plot.chart == TRUE) {
    format_control_chart(pct) + 
      geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff, colour = '#00BB00', linetype = 1) +
      scale_x_date(labels = date_format("%Y-%m"), breaks = cht_axis_breaks,
                   limits = c(q.st.dt, q.ed.dt)) +
      annotate("text", ed.dt - 90, 95, vjust = -2, label = "95% Target", colour = '#00BB00') +
      ggtitle(cht_title, subtitle = pr_name) +
      labs(x= x_title, y="Percentage within 4 hours") +
      ylim(ylimlow,100) +
      geom_text(aes(label=ifelse(x==max(x), format(x, '%b-%y'),'')),hjust=-0.05, vjust= 2)
  } else {df}
}


plot_volume <- function(df, prov_codes = c("RBZ"), date.col = 'Month_Start',
                             start.date = "2015-07-01", end.date = "2018-05-30",
                             brk.date = NULL, max_lower_y_scale = 60,
                             measure = "All", plot.chart = TRUE,
                             pr_name = NULL, x_title = "Month") {
  
  cht_title = "Number of A&E attendances"
  
  df <- make_perf_series(df = df, prov_codes = prov_codes, measure = measure)
  
  # if no pr_name passed, lookup full name of provider
  if (is.null(pr_name)) {pr_name <- df %>% top_n(1, wt = Performance) %>% pull(Prov_Name)}
  
  # convert arguments to dates and round to nearest quarter
  st.dt <- as.Date(start.date, tz = "Europe/London")
  ed.dt <- as.Date(end.date, tz = "Europe/London")
  q.st.dt <- as.Date(zoo::as.yearqtr(st.dt, format="%Y-%m-%d"))
  q.ed.dt <- as.Date(zoo::as.yearqtr(ed.dt, format="%Y-%m-%d"), frac = 1) + 1
  cht_axis_breaks <- seq(q.st.dt, q.ed.dt, "quarters")
  
  # restrict to the period specified
  df <- df %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  if(nrow(df)==0) {stop("No data for provider period specified")}

  if (is.null(brk.date)) {
    pct <- qicharts2::qic(Month_Start, Total, n = rep(1, nrow(df)), data = df, chart = 'up')
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    pct <- ggplot(pct$data, aes(x,y))
  } else {
    br.dt <- as.Date(brk.date)
    # locate break row
    v <- df$Month_Start
    br.row <- which(v == max(v[v < br.dt]))
    
    pct <- qicharts2::qic(Month_Start, Total, n = rep(1, nrow(df)), data = df, chart = 'up',
                          freeze = br.row)
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    pct <- ggplot(pct$data, aes(x,y))
  }
  
  # chart y limit
  ylimlow <- 0
  ylimhigh <- 1000*ceiling(max(df$Total)*1.1/1000)
  
  cutoff <- data.frame(yintercept=95, cutoff=factor(95))
  
  if(plot.chart == TRUE) {
    format_control_chart(pct) + 
      scale_x_date(labels = date_format("%Y-%m"), breaks = cht_axis_breaks,
                   limits = c(q.st.dt, q.ed.dt)) +
      ggtitle(cht_title, subtitle = pr_name) +
      labs(x= x_title, y="Number of attendances") +
      scale_y_continuous(limits = c(ylimlow, ylimhigh),
                         breaks = seq(ylimlow, ylimhigh, 1000*round((ylimhigh-ylimlow)/8/1000)))
  } else {df}
}


format_control_chart <- function(cht) {
  cht + 
    geom_line(colour = "black", size = .5) + 
    geom_line(aes(x,cl), size = 0.75) +
    geom_line(aes(x,ucl), size = 0.75, linetype = 2) +
    geom_line(aes(x,lcl), size = 0.75, linetype = 2) +
    geom_point(colour = "black" , fill = "black", size = 2) +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey80"),
              panel.grid.minor = element_blank(), panel.background = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
              axis.text.y = element_text(size = 14), axis.title = element_text(size = 14),
              plot.title = element_text(size = 20, hjust = 0),
              plot.subtitle = element_text(size = 16, face = "italic"),
              axis.line = element_line(colour = "grey60"))
}
