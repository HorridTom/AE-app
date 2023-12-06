library(tidyverse)
library(dplyr)
library(qicharts2)
library(ggplot2)
library(scales)
library(zoo)
library(lubridate)
library(wktmo)

make_perf_series <- function(df, code = "RQM", measure = "All", level, 
                             weeklyOrMonthly = "Monthly", onlyProvsReporting = F) {
  
  df <- regional_analysis(df, level, onlyProvsReporting)
  df <- filter(df, Code == code)
  
  # Make new variables
    df <- df %>% mutate(Att_Typ1_NotBr = Att_Typ1 - Att_Typ1_Br,
                      Att_Typ2_NotBr = Att_Typ2 - Att_Typ2_Br,
                      Att_Typ3_NotBr = Att_Typ3 - Att_Typ3_Br,
                      Att_All_NotBr = Att_All - Att_All_Br,
                      E_Adm_Not4hBr_D = E_Adm_All_ED - E_Adm_4hBr_D) 
  

  if(df[1,"Nat_Code"] == "S"){
    perf_series <- df %>%
      select(Code, Month_Start, Name, Nat_Code,
             Within_4h = Att_All_NotBr, Greater_4h = Att_All_Br, Total_Att = Att_All,Total_Adm = E_Adm_All_ED)
    if(weeklyOrMonthly != "weekly"){
      perf_series <- weekly_to_monthly(perf_series)
    }
  }else{
    perf_series <- switch(measure,
                          All = df %>%
                            select(Code, Month_Start, Name, Nat_Code,
                                   Within_4h = Att_All_NotBr, Greater_4h = Att_All_Br, Total_Att = Att_All,
                                   Total_Adm = E_Adm_All_ED),
                          Typ1 = df %>%
                            select(Code, Month_Start, Name, Nat_Code,
                                   Within_4h = Att_Typ1_NotBr, Greater_4h = Att_Typ1_Br, Total_Att = Att_Typ1,
                                   Total_Adm = E_Adm_Typ1)#,
                          # Adm = df %>%
                          #   select(Code, Month_Start, Name, Nat_Code,
                          #          Within_4h = E_Adm_Not4hBr_D, Greater_4h = E_Adm_4hBr_D, Total = E_Adm_All_ED,
                          #          E_Adm_Typ1, E_Adm_All_ED)
    )
  }
  
  perf_series %>% mutate(Performance = Within_4h / Total_Att) %>%
    mutate(Month_Start = as.Date(Month_Start, tz = 'Europe/London')) %>%
    #two new cols: 
    #if National code - i.e. Scotland $ weekly, return number of days - i.e. 7
    #else return number of days in a months
    arrange(Month_Start) %>% mutate(days_in_period = ifelse(Nat_Code == "S" & weeklyOrMonthly == "weekly",7, days_in_month(df$Month_Start))) %>%
    mutate(daily_ave_att = Total_Att/days_in_period) %>%
    mutate(daily_ave_adm = Total_Adm/days_in_period)
  
}


regional_analysis <- function(df, level, onlyProvsReporting){
  
  if(level == "National"){
    Name <- "Country"
    Code <- "Nat_Code"
  }else if(level == "Regional"){
    Name <- "Region"
    Code <- "Reg_Code"
  }else{
    Name <- "Prov_Name"
    Code <- "Prov_Code"
  }
  
  Name <- as.name(Name)
  Code <- as.name(Code)
  
  df <- df %>%
      mutate(Name = !!Name, Code = !!Code)
  
  if(onlyProvsReporting == T & (level == "National" | level == "Regional")){
    provs_not_reporting <- df %>% filter(is.na(Att_All_Br)) %>% distinct(Prov_Code) %>% pull(Prov_Code)
    df <- df %>%
      filter(!(Prov_Code %in% provs_not_reporting)) 
    }

  dfReg <- df %>%
      group_by(Name, Code, Month_Start) %>%
      summarise(Att_Typ1 = sum(Att_Typ1, na.rm = TRUE),
                Att_Typ2 = sum(Att_Typ2, na.rm = TRUE),
                Att_Typ3 = sum(Att_Typ3, na.rm = TRUE),
                Att_All = sum(Att_All, na.rm = TRUE),
                Att_Typ1_Br = sum(Att_Typ1_Br),
                Att_Typ2_Br = sum(Att_Typ2_Br), Att_Typ3_Br = sum(Att_Typ3_Br), Att_All_Br = sum(Att_All_Br),
                Perf_Typ1 = (Att_Typ1 - Att_Typ1_Br)/Att_Typ1, Perf_All = (Att_All - Att_All_Br)/Att_All,
                E_Adm_Typ1 = sum(E_Adm_Typ1), E_Adm_Typ2 = sum(E_Adm_Typ2), E_Adm_Typ34 = sum(E_Adm_Typ34),
                E_Adm_All_ED = sum(E_Adm_All_ED), E_Adm_Not_ED = sum(E_Adm_Not_ED), E_Adm_All = sum(E_Adm_All),
                E_Adm_4hBr_D = sum(E_Adm_4hBr_D), E_Adm_12hBr_D = sum(E_Adm_12hBr_D),
                Nat_Code = dplyr::first(Nat_Code)) %>%
      ungroup()

}


#Reg_codes are made up so London=Lo, Midlands=Mi, North=No, South=So.  Make sure these aren't the same as Scottish Reg_Codes!
clean_region_col <- function(df){
  
  df <- df %>%
    mutate(Region = ifelse(str_detect(Region, coll("london",ignore_case = T)), "London", 
                              ifelse(str_detect(Region, coll("midlands",ignore_case = T)),"Midlands", 
                                     ifelse(str_detect(Region, coll("north",ignore_case = T)),"North of England", 
                                            "South of England"))),
           Reg_Code = ifelse(Region == "London", "Lo", 
                             ifelse(Region == "Midlands", "Mi", 
                                    ifelse(Region == "North of England", "No", "So"))), 
           Country = "England", Nat_Code = "E")
}

#function to out Scottish data into NHS England format to be used in the app
standardise_data <- function(df){
  
  df <- df %>%
    select(-c(Att_8hr_Br, Perf_8hr, Att_12hr_Br, Perf_12hr)) %>%
    rename(Region = Board_Name, Reg_Code = Board_Code, Att_All_Br = Att_4hr_Br, Perf_All = Perf_4hr, Month_Start = Week_End) %>%
    mutate(Country = "Scotland", Nat_Code = "S") %>%
    mutate(Att_Typ1 = NA, Att_Typ2 = NA, Att_Typ3 = NA, Att_Typ1_Br = NA, Att_Typ2_Br = NA, Att_Typ3_Br = NA,
           Perf_Typ1 = NA, E_Adm_Typ1 = NA, E_Adm_Typ2 = NA, E_Adm_Typ34 = NA, E_Adm_All_ED = NA, 
           E_Adm_Not_ED = NA, E_Adm_All = NA, E_Adm_4hBr_D = NA, E_Adm_12hBr_D = NA) %>%
    mutate(Region = gsub("NHS ","", Region))
  
}

weekly_to_monthly <- function(df){
  
  df <- mutate(df, Month_Start = as.Date(Month_Start))
  df <- df %>% arrange(Month_Start)
  datStart <- as.character(df$Month_Start[1] - weeks(1))  ###because dates are given as week_end
  
  Within_4h_df <- weekToMonth(df$Within_4h, datStart = datStart, wkMethod = "startDat", format = "%Y-%m-%d")
  Within_4h <- Within_4h_df$value
  Greater_4h_df <- weekToMonth(df$Greater_4h, datStart = datStart, wkMethod = "startDat", format = "%Y-%m-%d")
  Greater_4h <- Greater_4h_df$value
  TotalAtt_df <- weekToMonth(df$Total_Att, datStart = datStart, wkMethod = "startDat", format = "%Y-%m-%d")
  Total_Att <- TotalAtt_df$value
  TotalAdm_df <- weekToMonth(df$Total_Adm, datStart = datStart, wkMethod = "startDat", format = "%Y-%m-%d")
  Total_Adm <- TotalAdm_df$value
  Month_Start <- TotalAtt_df$yearMonth
  
  dfMonthly <- data.frame(Month_Start, Within_4h, Greater_4h, Total_Att, Total_Adm)
  dfMonthly <- dfMonthly %>%
    mutate(Code = df$Code[nrow(df)], Name = df$Name[nrow(df)], Nat_Code = df$Nat_Code[nrow(df)]) %>%
    filter(row_number() != 1 & row_number() != nrow(dfMonthly)) %>% 
    mutate(Month_Start = as.yearmon(Month_Start, tz = 'Europe/London'))
  
}

plot_performance <- function(df, code = "RBZ", date.col = 'Month_Start',
                             start.date = "2015-07-01", end.date = "2018-05-30",
                             brk.date = NULL, max_lower_y_scale = 60,
                             measure = "All", plot.chart = TRUE,
                             pr_name = NULL, x_title = "Month",
                             r1_col = "orange", r2_col = "steelblue3",
                             level = level, weeklyOrMonthly = "Monthly",
                             onlyProvsReporting = onlyProvsReporting) { 
  
  cht_title = "Percentage A&E attendances\nwith time in department < 4h"
  
  df <- make_perf_series(df = df, code = code, measure = measure, level = level, weeklyOrMonthly = weeklyOrMonthly, 
                         onlyProvsReporting = onlyProvsReporting)
  
  df <- filter(df, !(is.na(Within_4h))) 
  
  # if no pr_name passed, lookup full name of provider
  if (is.null(pr_name)) {pr_name <- df %>% top_n(1, wt = !!date.col) %>% pull(Name)}
  
  # convert arguments to dates
  st.dt <- as.Date(start.date, tz = "Europe/London")
  ed.dt <- as.Date(end.date, tz = "Europe/London")
  q.st.dt <- as.Date(zoo::as.yearqtr(st.dt, format="%Y-%m-%d"))
  q.ed.dt <- as.Date(zoo::as.yearqtr(ed.dt, format="%Y-%m-%d"), frac = 1) + 1
  cht_axis_breaks <- seq(q.st.dt, q.ed.dt, "quarters")
  
  # restrict to the period specified
  df <- df %>% filter(Month_Start >= st.dt, Month_Start <= ed.dt)
  if(nrow(df)==0) {stop("No data for provider period specified")}

  #currently only England differentiates by type
  if(df$Nat_Code[1] == "E"){
    typeTitle <- ifelse(measure == "Typ1", "\nType 1 departments only", "\nAll department types")
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

  if(onlyProvsReporting == T & (level == "National" | level == "Regional")){
    reportingTitle <- "\nIncludes only providers that are still reporting"
  }else if(onlyProvsReporting == F & (level == "National" | level == "Regional")){
    reportingTitle <- "\nIncludes providers that are no longer reporting"
  }else{
    reportingTitle <- ""
  }
  
  if(plot.chart == TRUE) {
    
    autospc::plot_auto_SPC(df, 
                           x = Month_Start, 
                           y = Within_4h, 
                           n = Total_Att, 
                           chartType = "P'",
                           title = cht_title,
                           subtitle = paste0(levelTitle, pr_name, typeTitle, reportingTitle),
                           override_x_title = x_title,
                           override_y_title = "Percentage within 4 hours")
    #still need caption = "*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any month outside the control limits \nRule 2: Eight or more consecutive months all above, or all below, the centre line"

  } else {df}
}


plot_volume <- function(df, code = "RBZ", date.col = 'Month_Start',
                             start.date = "2015-07-01", end.date = "2018-05-30",
                             brk.date = NULL, max_lower_y_scale = 60,
                             measure = "All", plot.chart = TRUE,
                             pr_name = NULL, x_title = "Month",
                             r1_col = "orange", r2_col = "steelblue3", level = "Provider", 
                             weeklyOrMonthly = "Monthly",
                             onlyProvsReporting = onlyProvsReporting,
                             attOrAdm = "Attendances") { 
  
  if(attOrAdm == "Attendances"){
    cht_title <- ifelse(weeklyOrMonthly == "weekly", "Average daily A&E attendances per week", "Average daily A&E attendances per month")
  }else{
    cht_title <- ifelse(weeklyOrMonthly == "weekly", "Average daily admissions from A&E per week", "Average daily admissions from A&E per month")
  }

  
  df_original <- df
  df <- make_perf_series(df = df_original, code = code, measure = measure, level = level, 
                         weeklyOrMonthly = weeklyOrMonthly, onlyProvsReporting = onlyProvsReporting)
  df_all <- make_perf_series(df = df_original, code = code, measure = measure, level = level, 
                         weeklyOrMonthly = weeklyOrMonthly, onlyProvsReporting = FALSE)
  
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

  # for subtitle 
  if(df$Nat_Code[1] == "E"){
    typeTitle <- ifelse(measure == "Typ1", "\nType 1 departments only", "\nAll department types")
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
  
  if(onlyProvsReporting == T & (level == "National" | level == "Regional")){
    reportingTitle <- "\nIncludes only providers that are still reporting"
  }else if(onlyProvsReporting == F & (level == "National" | level == "Regional")){
    reportingTitle <- "\nIncludes providers that are no longer reporting"
  }else{
    reportingTitle <- ""
  }
  
  if(plot.chart == TRUE) {
    
    if(attOrAdm == "Attendances"){qicharts2::qic(Month_Start, daily_ave_att, n = rep(1, nrow(df)), data = df, chart = 'up')
      autospc::plot_auto_SPC(df, 
                             x = Month_Start,
                             y = daily_ave_att,
                             chartType = "C'",
                             title = cht_title,
                             subtitle = paste0(levelTitle, pr_name, typeTitle, reportingTitle),
                             override_x_title = x_title,
                             override_y_title = ifelse(attOrAdm == "Attendances","Average daily attendances", "Average daily admissions"))
      #need caption = "*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any month outside the control limits 
      #\nRule 2: Eight or more consecutive months all above, or all below, the centre line"
    }else{
      qicharts2::qic(Month_Start, daily_ave_att, n = rep(1, nrow(df)), data = df, chart = 'up')
      autospc::plot_auto_SPC(df, 
                             x = Month_Start,
                             y = daily_ave_adm,
                             chartType = "C'",
                             title = cht_title,
                             subtitle = paste0(levelTitle, pr_name, typeTitle, reportingTitle),
                             override_x_title = x_title,
                             override_y_title = ifelse(attOrAdm == "Attendances","Average daily attendances", "Average daily admissions"))
      #need caption = "*Shewhart chart rules apply (see Understanding the Analysis tab for more detail) \nRule 1: Any month outside the control limits 
      #\nRule 2: Eight or more consecutive months all above, or all below, the centre line"
      }
    

    
  } else {df}
}
