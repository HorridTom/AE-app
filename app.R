
library(shiny)
library(tidyverse)
library(qicharts)
library(RColorBrewer)
library(reshape2)
library(scales)
library(stringr)

library(nhsAEscraper)

Sys.setenv(TZ='Europe/London')
source("perf_4h_analysis.R")

update_data = TRUE
urls_of_data <- getAEdata_urls_monthly()
AE_Data <- getAE_data(update_data = update_data, directory = 'data-raw')
sitrep_perf_df <- make_p4h_from_sitreps(AE_Data)
assign("sitrep_perf", sitrep_perf_df, envir = .GlobalEnv)
assign("urls_of_data_obtained", urls_of_data, envir = .GlobalEnv)

# Define UI
ui <- fluidPage(
   
   # Logo
   img(src = "CLAHRC-logo.png", height = 60, width = 200),
  
   # Application title
   titlePanel("NHS England Trusts 4hr Performance Over Time"),
   
   sidebarLayout(
      sidebarPanel(
        p("This application provides statistical process control analysis of
          accident and emergency data for English NHS Trusts."),
          uiOutput("orgControl"),
          uiOutput("t1Control"),
          HTML("<br/>"),
          HTML("<br/>"),
          p("This analysis uses p-prime and u-prime charts, more information
          is available here:"),
          a("Prime charts publication", href="http://dx.doi.org/10.1136/qshc.2006.017830"),
          HTML("<br/>"),
          HTML("<br/>"),
          p("All the data used
          is publicly available from the NHS England website:"),
          a("A&E waiting times and activity", href="https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/")
      ),
      
      # Show a plot
      mainPanel(
        plotOutput("edPerfPlot"),
        plotOutput("edVolPlot")
      )
      
   )
)

# Define server logic
server <- function(input, output) {
  
  # If new data has been released since the app was launched,
  # download it
  current_data_urls <- getAEdata_urls_monthly()
  if (!setequal(urls_of_data_obtained, current_data_urls)) {
    file.remove(
      dir('data-raw',
          pattern = "*",
          full.names = TRUE)
      )
    AE_Data <- getAE_data(update_data = TRUE, directory = 'data-raw')
    sitrep_perf_df <- make_p4h_from_sitreps(AE_Data)
    
    assign("sitrep_perf", sitrep_perf_df, envir = .GlobalEnv)
    assign("urls_of_data_obtained", current_data_urls, envir = .GlobalEnv)
  }
  
  provLookup <- sitrep_perf[!duplicated(sitrep_perf[,c('Prov_Code')]),c('Prov_Code','Prov_Name')]
  provLookup <- provLookup %>% arrange(Prov_Name)
  orgs <- provLookup$Prov_Code
  orgNames <- provLookup$Prov_Name
  
  perf.start.date <- "2015-06-01"
  perf.end.date <- lubridate::today()
  perf.brk.date <- NULL
  
  output$orgControl <- renderUI({
    selectInput("trust", "Choose Trust", orgNames)
  })
  
  output$t1Control <- renderUI({
      checkboxInput("t1_only_checkbox", label = "Only include type 1 departments", value = FALSE)
  })
   
   output$edPerfPlot <- renderPlot({
     if (length(input$trust) != 0) {
      pr <- c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Code'][[1,1]])
      dept_types <- c('1','2','3')
      if(input$t1_only_checkbox) {dept_types = c('1')}
      tryCatch(plot_performance(sitrep_perf, prov_codes = pr, start.date = perf.start.date, end.date = perf.end.date,
                                brk.date = perf.brk.date, dept_types = dept_types, date.col = 'Month_Start',
                                x_title = "Month", adm_only = FALSE),
               error=function(e) NULL)
     }
    })
   
   output$edVolPlot <- renderPlot({
     if (length(input$trust) != 0) {
       pr <- c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Code'][[1,1]])
       dept_types <- c('1','2','3')
       if(input$t1_only_checkbox) {dept_types = c('1')}
       tryCatch(plot_volume(sitrep_perf, prov_codes = pr, start.date = perf.start.date, end.date = perf.end.date,
                            brk.date = perf.brk.date, dept_types = dept_types, date.col = 'Month_Start',
                            x_title = "Month", adm_only = FALSE),
                error=function(e) NULL)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

