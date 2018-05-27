library(tidyverse)
library(ggplot2)
library(scales)

make_perf_series <- function(df, prov_codes = c("RQM"), measure = "All") {
  
  df <- df %>% filter(Prov_Code %in% prov_codes)
  df <- make_new_variables(df)
  
  perf_series <- switch(measure,
         All = df %>%
           select(Prov_Code, Month_Start,
                  Within_4h = Att_All_NotBr, Greater_4h = Att_All_Br, Total = Att_All),
         Typ1 = df %>%
           select(Prov_Code, Month_Start,
                  Within_4h = Att_Typ1_NotBr, Greater_4h = Att_Typ1_Br, Total = Att_Typ1),
         Adm = df %>%
           select(Prov_Code, Month_Start,
                  Within_4h = E_Adm_Not4hBr_D, Greater_4h = E_Adm_4hBr_D, Total = E_Adm_All_ED)
           )
  perf_series %>% mutate(Performance = Within_4h / Total)
}


plot_performance <- function(df, prov_codes = c("RBZ"), date.col = 'Month_Start',
                             start.date = "2014-01-01", end.date = "2017-06-30",
                             brk.date = "2016-02-01", max_lower_y_scale = 60,
                             all_provs = FALSE,
                             measure = "All", plot.chart = TRUE,
                             pr_name = NULL, x_title = "Week Ending Sunday") {

  # if no pr_name passed, lookup full name of provider
  # note written for just one provider
  if (is.null(pr_name)) {
    pr_name <- df[which(df$Prov_Code == prov_codes),"Prov_Name"][[1]]
  }
  
  cht_title = paste("Percentage A&E attendances\nwith time in department < 4h",sep="")
  
  df <- make_perf_series(df = df, prov_codes = prov_codes, measure = measure)
  df$Month_Start <- as.Date(df$Month_Start)
  
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  
  # restrict to the period specified
  df <- df %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  if(nrow(df)==0) {stop("No data for provider period specified")}
  # This is a hack - find better way to modify colours of qicharts
  # Also needs stepped limits
  
  if (is.null(brk.date)) {
    pct <- qicharts::tcc(n = Within_4h, d = df$Total, x = df$Month_Start, data = df, chart = 'p', multiply = 100, prime = TRUE, runvals = TRUE, cl.lab = FALSE)
  } else {
    br.dt <- as.Date(brk.date)
    # locate break row
    v <- df$Month_Start
    br.row <- which(v == max(v[v < br.dt]))
    
    pct <- qicharts::tcc(n = Within_4h, d = df$Total, x = df$Month_Start, data = df, chart = 'p', multiply = 100, prime = TRUE, breaks = c(br.row), runvals = TRUE, cl.lab = FALSE)
  }
  # chart y limit
  ylimlow <- min(min(pct$data$y, na.rm = TRUE),min(pct$data$lcl, na.rm = TRUE), max_lower_y_scale)
  
  col1    <- rgb(000, 000, 000, maxColorValue = 255)
  col2    <- rgb(241, 088, 084, maxColorValue = 255)
  col3    <- rgb(000, 000, 000, maxColorValue = 255)
  col4    <- 'white'
  col5    <-  rgb(096, 189, 104, maxColorValue = 255)
  cols    <- c('col1' = col1, 'col2' = col2, 'col3' = col3, 'col4' = col4)
  
  cutoff <- data.frame(yintercept=95, cutoff=factor(95))

  if(plot.chart == TRUE) {pct + geom_line(aes_string(x = 'x', y = 'lcl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
    geom_line(aes_string(x = 'x', y = 'ucl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
    geom_line(aes_string(x = 'x', y = 'cl', group = 'breaks'), colour = '#000000', linetype = 1) +
    geom_line(aes_string(x = 'x', y = 'y', group = 'breaks'), colour = '#000000', linetype = 1, lwd = 1.1) + 
    geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff, colour = '#00BB00', linetype = 1)  +
    annotate("text", ed.dt - 90, 95, vjust = -2, label = "95% Target", colour = '#00BB00') +
    geom_point(aes_string(x = 'x', y = 'y', group = 'breaks', fill = 'pcol'), size = 2) + 
    scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
    ggtitle(cht_title, subtitle = pr_name) +
    labs(x= x_title, y="Percentage") +
    ylim(ylimlow,100) + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("3 months"), limits = as.Date(c(start.date, end.date))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
          axis.text.y = element_text(size = 14), axis.title = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0),
          plot.subtitle = element_text(size = 16, face = "italic"),
          axis.line = element_line(colour = "grey60"))
  } else {df}
    
}


plot_volume <- function(df, prov_codes = c("RBZ"), date.col = 'Month_Start',
                             start.date = "2014-01-01", end.date = "2017-06-30",
                             brk.date = "2016-02-01", max_lower_y_scale = 60,
                             all_provs = FALSE,
                             measure = "All", plot.chart = TRUE,
                             pr_name = NULL, x_title = "Week Ending Sunday") {
  
  # if no pr_name passed, lookup full name of provider
  # note written for just one provider
  if (is.null(pr_name)) {
    pr_name <- df[which(df$Prov_Code == prov_codes),"Prov_Name"][[1]]
  }
  
  cht_title = paste("Percentage A&E attendances\nwith time in department < 4h",sep="")
  
  df <- make_perf_series(df = df, prov_codes = prov_codes, measure = measure)
  df$Month_Start <- as.Date(df$Month_Start)
  
  st.dt <- as.Date(start.date)
  ed.dt <- as.Date(end.date)
  
  # restrict to the period specified
  df <- df %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  
  # This is a hack - find better way to modify colours of qicharts
  # Also needs stepped limits
  
  if (is.null(brk.date)) {
    pct <- qicharts::tcc(n = Total, d = rep(1, nrow(df)), x = df$Month_Start, data = df, chart = 'u', multiply = 1, prime = TRUE, runvals = TRUE, cl.lab = FALSE)
  } else {
    br.dt <- as.Date(brk.date)
    # locate break row
    v <- df[,date.col]
    br.row <- which(v == max(v[v < br.dt]))
    
    pct <- qicharts::tcc(n = Total, d = rep(1, nrow(df)), x = df$Month_Start, data = df, chart = 'u', multiply = 1, prime = TRUE, breaks = c(br.row), runvals = TRUE, cl.lab = FALSE)
  }
  # chart y limit
  ylimlow <- 0
  ylimhigh <- 1000*ceiling(max(df$Total)*1.1/1000)
  
  col1    <- rgb(000, 000, 000, maxColorValue = 255)
  col2    <- rgb(241, 088, 084, maxColorValue = 255)
  col3    <- rgb(000, 000, 000, maxColorValue = 255)
  col4    <- 'white'
  col5    <-  rgb(096, 189, 104, maxColorValue = 255)
  cols    <- c('col1' = col1, 'col2' = col2, 'col3' = col3, 'col4' = col4)
  
  cutoff <- data.frame(yintercept=95, cutoff=factor(95))
  
  if(plot.chart == TRUE) {pct + geom_line(aes_string(x = 'x', y = 'lcl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
      geom_line(aes_string(x = 'x', y = 'ucl', group = 'breaks'), colour = '#000000', linetype = 'dashed') +
      geom_line(aes_string(x = 'x', y = 'cl', group = 'breaks'), colour = '#000000', linetype = 1) +
      geom_line(aes_string(x = 'x', y = 'y', group = 'breaks'), colour = '#000000', linetype = 1, lwd = 1.1) + 
      geom_point(aes_string(x = 'x', y = 'y', group = 'breaks', fill = 'pcol'), size = 2) + 
      scale_fill_manual(values = cols) + scale_color_manual(values = cols) +
      ggtitle(cht_title) +
      labs(x= x_title, y="Count") +
      ylim(ylimlow, ylimhigh) + scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("3 months"), limits = as.Date(c(start.date, end.date))) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
            axis.text.y = element_text(size = 14), axis.title = element_text(size = 14),
            plot.title = element_text(size = 20, hjust = 0),
            plot.subtitle = element_text(size = 16, face = "italic"),
            axis.line = element_line(colour = "grey60"))
  } else {df}
  
}


make_new_variables <- function(AE_data) {
  
  AE_data <- AE_data %>% mutate(Att_Typ1_NotBr = Att_Typ1 - Att_Typ1_Br,
                                Att_Typ2_NotBr = Att_Typ2 - Att_Typ2_Br,
                                Att_Typ3_NotBr = Att_Typ3 - Att_Typ3_Br,
                                Att_All_NotBr = Att_All - Att_All_Br,
                                E_Adm_Not4hBr_D = E_Adm_All_ED - E_Adm_4hBr_D)
  
  AE_data
  
}
