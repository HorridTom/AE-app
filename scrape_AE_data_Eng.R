getAE_data <- function(update_data = TRUE) {
  
  if(update_data) {
    urls <- getAEdata_urls_monthly()
    download_AE_files(urls)
  }
  rawDataList <- load_AE_files()
  
  rawDataList <- lapply(rawDataList, delete_extra_columns)
  
  if(!all(unlist(lapply(rawDataList, check_format)))) {
    stop('There is a problem with the format of the data in one or more of the files')
  }
  
  neatDataList <- lapply(rawDataList, tidy_AE_data)
  
  AE_data <- dplyr::bind_rows(neatDataList)
  
  AE_data
  
}


getAEdata_urls_monthly <- function() {
  
  #This function returns the urls for NHS England A&E data *.xls files from two pages
  #yielding addresses for monthly data from June 2015 to (in principle) present.
  
  url_15_16 <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/statistical-work-areasae-waiting-times-and-activityae-attendances-and-emergency-admissions-2015-16-monthly-3/"
  url_16_17 <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/statistical-work-areasae-waiting-times-and-activityae-attendances-and-emergency-admissions-2016-17/"
  url_17_18 <- "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2017-18/"
  
  url_list <- list(url_15_16, url_16_17, url_17_18)
  unlist(lapply(url_list,function(x) getAEdata_page_urls_monthly(x)))
  
}


getAEdata_page_urls_monthly <- function(index_url) {
  #This function returns the urls for NHS England A&E data *.xls files from one
  #of the monthly data index pages
  
  #Get the html from the index website
  con <- url(index_url, "r")
  html_lines <- readLines(con)
  
  #Close connection
  close(con)
  
  #Look for lines that contain the signature part of the url and the signature text
  data_url_lines <- grep("^(?=.*xls)((?!Quarter).)*$",html_lines, perl=TRUE)
  xlsdata_url_lines <- grep("AE-by-provider",html_lines[data_url_lines])
  NHSE_xlsdata_lines <- html_lines[data_url_lines][xlsdata_url_lines]
  
  #Extract urls from html lines
  starts <- regexpr("http",NHSE_xlsdata_lines)
  ends <- regexpr(".xls",NHSE_xlsdata_lines) + 3
  urls <- substr(NHSE_xlsdata_lines, starts, ends)
  
  #Return urls
  return(urls)
  
}


download_AE_files <- function(file_urls) {
  
  f_name_regex <- '/([^/]+)$'
  
  lapply(file_urls, function(x) {
    fn <- paste('data-raw/sitreps',stringr::str_match(x, f_name_regex)[,2],sep='/')
    download.file(x,fn, mode = 'wb')
  })
  
}


load_AE_files <- function(directory = 'data-raw/sitreps/', pkg = 'readxl') {
  
  fileNames <- Sys.glob(paste(directory,'*AE-by-provider*.xls',sep=''))
  
  dataList <- NULL
  if (pkg == 'readxl') {
    dataList <- lapply(fileNames, function(x) {readxl::read_excel(x, sheet = 1, col_names = FALSE)})
  } else if (pkg == 'gdata') {
    dataList <- lapply(fileNames, function(x) {
      y <- gdata::read.xls(x, as.is = TRUE)
      colnames(y) <- paste('X__',c(1:25),sep='')
      y
    })
  }
    
    dataList
}


tidy_AE_data <- function(raw_data) {
  
  data_date <- get_date(raw_data)
  
  neat_data <- raw_data %>% filter(grepl("^[A-Z0-9]+$",X__1))
  
  neat_data <- neat_data %>% select(X__1:X__21) %>%
    rename(Prov_Code = X__1,
                                    Region = X__2,
                                    Prov_Name = X__3,
                                    Att_Typ1 = X__4,
                                    Att_Typ2 = X__5,
                                    Att_Typ3 = X__6,
                                    Att_All = X__7,
                                    Att_Typ1_Br = X__8,
                                    Att_Typ2_Br = X__9,
                                    Att_Typ3_Br = X__10,
                                    Att_All_Br = X__11,
                                    Perf_Typ1 = X__12,
                                    Perf_All = X__13,
                                    E_Adm_Typ1 = X__14,
                                    E_Adm_Typ2 = X__15,
                                    E_Adm_Typ34 = X__16,
                                    E_Adm_All_ED = X__17,
                                    E_Adm_Not_ED = X__18,
                                    E_Adm_All = X__19,
                                    E_Adm_4hBr_D = X__20,
                                    E_Adm_12hBr_D = X__21)
  
  neat_data <- neat_data %>% mutate_at(vars(starts_with("Att_")), funs(as.numeric)) %>%
    mutate_at(vars(starts_with("Perf_")), funs(as.numeric)) %>%
    mutate_at(vars(starts_with("E_Adm_")), funs(as.numeric))
  
  neat_data <- neat_data %>% add_column(Month_Start = data_date, .after = 3)

  neat_data
}


check_format <- function(raw_data, verbose = FALSE) {
  
  format_status <- logical()
  
  format_status[1] <- nrow(raw_data %>% filter(X__1 == "Code")) == 1
  format_status[2] <- nrow(raw_data %>% filter(X__2 == "Region")) == 1
  format_status[3] <- nrow(raw_data %>% filter(X__3 == "Name")) == 1
  format_status[4] <- nrow(raw_data %>% filter(X__4 == "A&E attendances")) == 1
  format_status[5] <- nrow(raw_data %>% filter(grepl("A&E attendances > 4 hours from arrival to admission",X__8)|grepl("A&E attendances greater than 4 hours from arrival to admission",X__8))) == 1
  format_status[6] <- nrow(raw_data %>% filter(X__14 == "Emergency Admissions")) == 1
  
  
  if (verbose) {
    format_status
  } else {
      all(format_status)
    }
}


get_date <- function(raw_data) {
  #Find the cell specifying the period and extract the text
  date_chr <- raw_data %>% filter(X__1 == "Period:") %>%
    pull(X__2)
  lubridate::myd(paste(date_chr,'1st',sep=' '), tz = "Europe/London")
  
}


make_new_variables <- function(AE_data) {
  
  AE_data <- AE_data %>% mutate(Att_Typ1_NotBr = Att_Typ1 - Att_Typ1_Br,
                                Att_Typ2_NotBr = Att_Typ2 - Att_Typ2_Br,
                                Att_Typ3_NotBr = Att_Typ3 - Att_Typ3_Br,
                                Att_All_NotBr = Att_All - Att_All_Br,
                                E_Adm_Not4hBr_D = E_Adm_All_ED - E_Adm_4hBr_D)
  
  AE_data
  
}


make_p4h_from_sitreps <- function(AE_data) {
  
  # Add additional columns to make transformation simpler
  AE_data <- make_new_variables(AE_data)
  
  # Select only the columns needed for the combinations of flag variables
  AE_data <- AE_data %>% select(Prov_Code, Prov_Name, Region, Month_Start,
                           Att_Typ1_NotBr, Att_Typ1_Br, Att_Typ2_NotBr, Att_Typ2_Br,
                           Att_Typ3_NotBr, Att_Typ3_Br,
                           E_Adm_Not4hBr_D, E_Adm_4hBr_D)
  
  # Now gather all but org info and month
  df <- AE_data %>% gather(key, value, -Prov_Code, -Prov_Name, -Region, -Month_Start)
  
  # Create flag columns
  df$Admitted <- grepl('E_Adm*', df$key)
  df$Greater_4h <- grepl('*_Br*|*_4hBr_*', df$key)
  df$Greater_12h <- NA
  df$AEA_Department_Type <- str_match(df$key, 'Typ([0-9])_')[,2]
  
  # Rename
  df <- df %>% rename(Activity = value)
  
  # Remove redundant key column
  df$key <- NULL
  
  # Convert to date
  df$Month_Start <- as.Date(df$Month_Start, tz = 'Europe/London')
  
  df
}


delete_extra_columns <- function(df) {
  format_type_x <- nrow(df %>% filter(grepl("A&E attendances less than 4 hours from arrival to admission",X__8))) == 1
  if(!format_type_x) return(df)
  df <- df %>% select(-c(X__8,X__9,X__10,X__11))
  colnames(df) <- paste('X__',c(1:ncol(df)),sep='')
  df
}