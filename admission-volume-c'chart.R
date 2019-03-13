
plot_adm_volume <- function(df, code = "RBZ", date.col = 'Month_Start',
                        start.date = "2015-07-01", end.date = "2018-05-30",
                        brk.date = NULL, max_lower_y_scale = 60,
                        measure = "Adm", plot.chart = TRUE,
                        pr_name = NULL, x_title = "Month",
                        r1_col = "orange", r2_col = "steelblue3", level = "Provider", 
                        weeklyOrMonthly = "Monthly") { 
  
  #cht_title = "Number of A&E admissions"
  cht_title <- ifelse(weeklyOrMonthly == "weekly", "Number of A&E admissions per week", "Number of A&E admissions per month")
  
  df <- make_perf_series(df = df, code = code, measure = measure, level = level, weeklyOrMonthly = weeklyOrMonthly)
  
  # if no pr_name passed, lookup full name of provider
  if (is.null(pr_name)) {pr_name <- df %>% top_n(1, wt = Performance) %>% pull(Name)}
  
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
    pct <- qicharts2::qic(Month_Start, total, n = rep(1, nrow(df)), data = df, chart = 'up')
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    cht_data <- add_rule_breaks(pct$data)
    pct <- ggplot(cht_data, aes(x,y))
  } else {
    br.dt <- as.Date(brk.date)
    # locate break row
    v <- df$Month_Start
    br.row <- which(v == max(v[v < br.dt]))
    
    pct <- qicharts2::qic(Month_Start, total, n = rep(1, nrow(df)), data = df, chart = 'up',
                          freeze = br.row)
    pct$data$x <- as.Date(pct$data$x, tz = 'Europe/London')
    cht_data <- add_rule_breaks(pct$data)
    pct <- ggplot(cht_data, aes(x,y))
    
  }
  
  # chart y limit
  ylimlow <- 0
  ylimhigh <- 1000*ceiling(max(df$total)*1.1/1000)
  
  cutoff <- data.frame(yintercept=95, cutoff=factor(95))
  
  # for subtitle 
  if(df$Nat_Code[1] == "E"){
    typeTitle <- ifelse(measure == "Typ1", "\n(Type 1 departments only)", "\n(All department types)")
  }else{
    typeTitle <- ""
  }
  
  if(level == "National"){
    levelTitle <- "Country:"
  }else if(level == "Regional"){
    levelTitle <- ifelse(df$Nat_Code[1] == "E", "Region:","Board:")
  }else{
    levelTitle <- ""
  }
  
  if(plot.chart == TRUE) {
    format_control_chart(pct, r1_col = r1_col, r2_col = r2_col) + 
      scale_x_date(labels = date_format("%Y-%m"), breaks = cht_axis_breaks,
                   limits = c(q.st.dt, q.ed.dt)) +
      ggtitle(cht_title, subtitle = paste(levelTitle,pr_name, typeTitle)) + 
      labs(x= x_title, y="Number of admissions",
           caption = "*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any month outside the control limits \nRule 2: Eight or more consecutive months all above, or all below, the centre line",
           size = 10) +
      scale_y_continuous(limits = c(ylimlow, ylimhigh),
                         label = comma) 
    
  } else {df}
}

format_control_chart <- function(cht, r1_col, r2_col) {
  point_colours <- c("Rule 1" = r1_col, "Rule 2" = r2_col, "None" = "black")
  cht + 
    geom_line(colour = "black", size = .5) + 
    geom_line(aes(x,cl), size = 0.75) +
    geom_line(aes(x,ucl), size = 0.75, linetype = 2) +
    geom_line(aes(x,lcl), size = 0.75, linetype = 2) +
    geom_point(aes(colour = highlight), size = 2) +
    scale_color_manual("Rule triggered*", values = point_colours) + 
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "grey80"),
          panel.grid.minor = element_blank(), panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0, size = 14),
          axis.text.y = element_text(size = 14), axis.title = element_text(size = 14),
          plot.title = element_text(size = 20, hjust = 0),
          plot.subtitle = element_text(size = 16, face = "italic"),
          axis.line = element_line(colour = "grey60"),
          plot.caption = element_text(size = 10, hjust = 0.5))
}