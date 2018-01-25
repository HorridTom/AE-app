
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
   
   # Application title
   titlePanel("NHS England Trusts 4hr Performance Over Time"),
   img(src = "CLAHRC-logo.png", height = 60, width = 200),
   
   sidebarLayout(
      sidebarPanel(
        p("This application provides statistical process control analysis of
          accident and emergency data for English NHS Trusts. All the data used
          is publicly available from the NHS England website:"),
        a("A&E waiting times and activity", href="https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/"),
        HTML("<br/>"),
        HTML("<br/>"),
        uiOutput("orgControl")
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
  perf.end.date <- "2018-01-01"
  perf.brk.date <- "2016-07-01"
  
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

