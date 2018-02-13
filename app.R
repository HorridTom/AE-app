
library(shiny)
library(tidyverse)
library(qicharts)
library(RColorBrewer)
library(reshape2)
library(scales)
Sys.setenv(TZ='Europe/London')
source("scrape_AE_data_Eng.R")
source("perf_4h_analysis.R")

update_data = TRUE

AE_Data <- getAE_data(update_data = update_data)
sitrep_perf_df <- make_p4h_from_sitreps(AE_Data)

assign("sitrep_perf", sitrep_perf_df, envir = .GlobalEnv)

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
          HTML("<br/>"),
          HTML("<br/>"),
          p("The analysis method used is the p-prime chart, more information
          is available here:"),
          a("p-prime charts publication", href="http://dx.doi.org/10.1136/qshc.2006.017830"),
          HTML("<br/>"),
          HTML("<br/>"),
          p("All the data used
          is publicly available from the NHS England website:"),
          a("A&E waiting times and activity", href="https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/")
      ),
      
      # Show a plot
      mainPanel(
        plotOutput("edPerfPlot")
      )
      
   )
)

# Define server logic
server <- function(input, output) {
  
  
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
   
   output$edPerfPlot <- renderPlot({
     if (length(input$trust) != 0) {
      pr <- c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Code'][[1,1]])
      tryCatch(plot_performance(sitrep_perf, prov_codes = pr, start.date = perf.start.date, end.date = perf.end.date, brk.date = perf.brk.date, dept_types = NA, date.col = 'Month_Start', x_title = "Month"), error=function(e) NULL)
     }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

