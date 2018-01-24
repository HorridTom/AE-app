#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(qicharts)
library(RColorBrewer)
library(reshape2)
library(scales)
Sys.setenv(TZ='Europe/London')
source("scrape_AE_data_Eng.R")
source("perf_4h_analysis.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NHS England Trusts 4hr Performance Over Time"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        uiOutput("orgControl")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("edPerfPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  update_data = TRUE
  
  AE_Data <- getAE_data(update_data = update_data)
  sitrep_perf <- make_p4h_from_sitreps(AE_Data)
  
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
     pr <- c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Code'][[1,1]])
     tryCatch(plot_performance(sitrep_perf, prov_codes = pr, start.date = perf.start.date, end.date = perf.end.date, brk.date = perf.brk.date, dept_types = NA, date.col = 'Month_Start', x_title = "Month"), error=function(e) NULL)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

