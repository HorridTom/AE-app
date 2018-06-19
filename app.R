library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)

library(nhsAEscraper)

Sys.setenv(TZ='Europe/London')
source("perf_4h_analysis.R")

update_data = TRUE
urls_of_data <- getAEdata_urls_monthly()
AE_Data <- getAE_data(update_data = update_data, directory = 'data-raw')
assign("AE_Data", AE_Data, envir = .GlobalEnv)
assign("urls_of_data_obtained", urls_of_data, envir = .GlobalEnv)

# Define UI
ui <- dashboardPage(

   # Application title
   dashboardHeader(tags$li(class = "dropdown",
                           tags$style(".main-header {max-height: 90px}"),
                           tags$style(".main-header .logo {height: 90px;}"),
                           tags$style(".sidebar-toggle {height: 90px; padding-top: 1px !important;}"),
                           tags$style(".navbar {min-height:90px !important}")
                          ),
                    title = "A&E Charts",
                    titleWidth = 300,
                    dropdownMenu(type = "notifications", badgeStatus = NULL, headerText = NULL,
                                  icon = img(src="CLAHRC-logo-white.png", height = 60, width = 190))
    ),
   dashboardSidebar(tags$style(".left-side, .main-sidebar {padding-top: 90px}"),
                    width = 300,
      sidebarMenuOutput("menu")
    ),
   
   dashboardBody(
    # No CSS styling for now
    #tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #),
     tabItems(
       tabItem(tabName = "analysis",
                h1("Analysis of Accident and Emergency Attendance Data"),
                h4("NHS England Provider Organisations"),
                fluidRow(column(width = 12,
                                box(plotOutput("edPerfPlot"), width = NULL),
                                box(plotOutput("edVolPlot"), width = NULL)
                                )
                         )
        ),
        tabItem(tabName = "understanding",
                h1("Understanding the analysis"),
                p("This application provides statistical process control analysis of
                  accident and emergency data for English NHS Trusts."),
                br(),
                p("This analysis uses p-prime and u-prime charts, more information
                  is available here:"),
                a("Prime charts publication", href="http://dx.doi.org/10.1136/qshc.2006.017830"),
                br(),
                p("All the data used
                is publicly available from the NHS England website:"),
                a("A&E waiting times and activity",
                  href="https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/")
        ),
       tabItem(tabName = "dev",
               h1("Development"))
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
    assign("AE_Data", AE_Data, envir = .GlobalEnv)
    assign("urls_of_data_obtained", current_data_urls, envir = .GlobalEnv)
  }
  
  provLookup <- AE_Data[!duplicated(AE_Data[,c('Prov_Code')]),c('Prov_Code','Prov_Name')]
  provLookup <- provLookup %>% arrange(Prov_Name)
  orgs <- provLookup$Prov_Code
  orgNames <- provLookup$Prov_Name
  
  perf.start.date <- "2015-07-01"
  perf.end.date <- lubridate::today()
  perf.brk.date <- NULL
  
  output$menu <- renderMenu({
    sidebarMenu(id = "tabs",
      menuItem("Analyse A&E data", tabName = "analysis", icon = icon("hospital-o", lib = "font-awesome")),
      conditionalPanel(condition = "input.tabs === 'analysis'",
                       selectInput("trust", "Choose Trust", orgNames),
                       checkboxInput("t1_only_checkbox", label = "Only include type 1 departments",
                                     value = FALSE)
      ),
      menuItem("Understanding the analysis", tabName = "understanding", icon = icon('info-circle')),
      menuItem("Development", tabName = "dev", icon = icon('road'))
    )
  })

   output$edPerfPlot <- renderPlot({
     if (length(input$trust) != 0) {
      pr <- c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Code'][[1,1]])
      measure <- "All"
      if(input$t1_only_checkbox) {measure <- "Typ1"}
      tryCatch(plot_performance(AE_Data, prov_codes = pr, start.date = perf.start.date, end.date = perf.end.date,
                                brk.date = perf.brk.date, date.col = 'Month_Start',
                                x_title = "Month", measure = measure),
               error=function(e) NULL)
     }
    })
   
   output$edVolPlot <- renderPlot({
     if (length(input$trust) != 0) {
       pr <- c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Code'][[1,1]])
       measure <- "All"
       if(input$t1_only_checkbox) {measure <- "Typ1"}
       tryCatch(plot_volume(AE_Data, prov_codes = pr, start.date = perf.start.date, end.date = perf.end.date,
                            brk.date = perf.brk.date, date.col = 'Month_Start',
                            x_title = "Month", measure = measure),
                error=function(e) NULL)
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
