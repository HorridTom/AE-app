library(shiny)
library(nhsAEscraper) #devtools::install_github("HorridTom/nhsAEscraper", ref = "enhancement-cloud-compatibility")
library(RMySQL)
library(DBI)
library(tidyverse)
library(openssl)
library(mailR)

#Function for getting new connection to Cloud SQL
get_SQL_connection <- function(){
  conn <-
    dbConnect(
      RMySQL::MySQL(),
      user = config::get("user"),
      password = config::get("password"),
      host = config::get("host"),
      dbname = config::get("dbname"),
      port = config::get("port")
    )
  return(conn)
}

#function to update both tables for each country in the database
update_database <- function(data, country, conn){
  
  #add a time stamp column 
  AE_Data_timestamp <- data %>%
    mutate(downloadDatetime = Sys.time())
  
  #Fetch archived data from database and load into environment
  if(country == "England"){
    res <- dbSendQuery(conn, "SELECT * from AE_Data_full")
    # AE_Data_full <- dbFetch(res, n=-1)
    # assign("AE_Data_full", AE_Data_full, envir = .GlobalEnv)
  }else{
    res <- dbSendQuery(conn, "SELECT * from AE_Data_Scot_full")
    # AE_Data_Scot_full <- dbFetch(res, n=-1)
    # assign("AE_Data_Scot_full", AE_Data_Scot_full, envir = .GlobalEnv)
  }
  AE_Data_full <- dbFetch(res, n=-1) 
  
  #create table of distinct filenames and contents hashs
  archivedFiles <- AE_Data_full %>%
    select(SourceFile, hashSourceFileContents) %>%
    distinct_all()
  
  #create a %notin% operator
  '%notin%' <- Negate('%in%')
  #get data rows which are not already archived
  AE_Data_newest <- AE_Data_timestamp %>%
    filter(hashSourceFileContents %notin% archivedFiles$hashSourceFileContents)
  
  
  #Append new data into database
  name <- ifelse(country == "England","AE_Data_full", "AE_Data_Scot_full")
  dbWriteTable(conn, name = name, value = AE_Data_newest, row.names = FALSE, append = TRUE)
  
  #Pull back updated full table 
  if(country == "England"){
    res <- dbSendQuery(conn, "SELECT * from AE_Data_full")
  }else{
    res <- dbSendQuery(conn, "SELECT * from AE_Data_Scot_full")
  }
  AE_Data_full <- dbFetch(res, n=-1)
  
  #Filter for latest versions of each org-time_period
  if(country == "England"){
    AE_Data_new <- group_by(AE_Data_full,Prov_Code,Month_Start) %>%
      filter(downloadDatetime == max(downloadDatetime)) %>%
      distinct(Prov_Code, Month_Start, .keep_all = T) %>%
      select(-c(SourceFile, hashSourceFileContents, downloadDatetime))
  }else{
    AE_Data_new <- group_by(AE_Data_full,Prov_Code,Week_End)%>%
      filter(downloadDatetime == max(downloadDatetime)) %>% 
      distinct(Prov_Code, Week_End, .keep_all = T) %>%
      select(-c(SourceFile, hashSourceFileContents, downloadDatetime))
  }
  
  ##populate AE_Data table with newest data
  name <- ifelse(country == "England","AE_Data", "AE_Data_Scot")
  dbWriteTable(conn, name = name, value = AE_Data_new, row.names = FALSE, overwrite = T)
  
  dbName <- config::get("dbname")
  
  ##number of new rows added
  if(country == "England"){
    #print the number of rows of the database
    res <- dbSendQuery(conn, paste0("SELECT COUNT(*) FROM ",dbName,".AE_Data_full"))
    print(paste("AE_Data_full count is",dbFetch(res)))
    
    noOfRowsAdded_AE_Data_full <- nrow(AE_Data_newest)
    return(noOfRowsAdded_AE_Data_full)
  }else{
    #print the number of rows of the database
    res <- dbSendQuery(conn, paste0("SELECT COUNT(*) FROM ",dbName,".AE_Data_Scot_full"))
    print(paste("AE_Data_Scot_full count is",dbFetch(res)))
    
    noOfRowsAdded_AE_Data_Scot_full <- nrow(AE_Data_newest)
    return(noOfRowsAdded_AE_Data_Scot_full)
  }
  
}


#function to return when the database was last updated 
last_update <- function(conn){
  
  res <- dbSendQuery(conn, "SELECT * from AE_Data_Scot_full")
  AE_Data_Scot_full <- dbFetch(res, n=-1)
  lastUpdateScot <- max(AE_Data_Scot_full$downloadDatetime)
  
  res <- dbSendQuery(conn, "SELECT * from AE_Data_full")
  AE_Data_full <- dbFetch(res, n=-1)
  lastUpdateEng <- max(AE_Data_full$downloadDatetime)
  
  lastUpdate <- max(lastUpdateEng, lastUpdateScot)
}


#function to send alert email to database admin when there is an update error
send_email <- function(country = "England", state){
  
  if(state == "success"){
    subject <- "Success in updating AE-App Database"
    body <- paste0("The A&E tracker database has successfully been updated for ", country,".")
  }else {
    subject <- "Error in updating AE-App Database"
    body <- paste0("There has been an error in updating the database for ", country, ". Please check to see what is wrong.  The log file has been attached for reference.")
  }
  
  send.mail(from = "aetrackermaintenance@gmail.com",
            to = "imogen.connor-helleur@imperial.ac.uk",
            subject = subject,
            body = body,
            html = T,
            smtp = list(host.name = "smtp.gmail.com",
                      port = 465,
                      user.name = "aetrackermaintenance@gmail.com",
                      passwd = "ARCNWL123",
                      ssl = T),
            authenticate = T#,
            #attach.files = "log.xlsx"
            )
  
  print("sent")
  
}

#function to update the database with error handling 
update_database_error_handle <- function(data, country, conn){
  
  #loop to retry if error and to send an email
  err <- T
  tries <- 1
  
  while(err == T & tries <= 3){
    tryCatch({
      rowsAdded <- update_database(data = data, country = country, conn = conn)
      #send_email(country = country, state = "success")
      err <- F
    }, warning = function(w) {
        err <- F
    }, error = function(e) {
        err <- T
        if(tries == 3){
          #send_email(country = country, state = "error")
        }
        print(e)
        #will wait 2mins before trying again (120s), currently waits 1s
        Sys.sleep(1)
    }, finally = {
        tries <- tries + 1
    })
  }
  
  #number of rows added to each database _full table 
  return(rowsAdded)
  
}
