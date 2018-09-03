library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)

###master branch 
library(nhsAEscraper)

Sys.setenv(TZ='Europe/London')
source("spc_rules.R")
source("perf_4h_analysis.R")

update_data = TRUE
r1_col = "orange"
r2_col = "steelblue3"

urls_of_data <- NULL
if(update_data) {urls_of_data <- getAEdata_urls_monthly()}
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
                    title = "A&E Tracker",
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
                         ),
               downloadButton('downloadPerfPlot', 'Download Performance Chart'), 
               downloadButton('downloadVolPlot', 'Download Attendances Chart')
        ),
        tabItem(tabName = "understanding",
                h1("Understanding the analysis"),
                p("This application provides statistical analysis of attendance data relating
                  to providers of NHS accident and emergency department services in England."),
                p("All the data used
                is publicly available from the NHS England website:"),
                a("A&E waiting times and activity",
                  href="https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/"),
                h2("Shewhart Charts"),
                p("The analysis uses Shewhart charts, also known as control charts. There
                  is a brief explanation of this approach below, for more information
                  please follow the links in the resources section.
                  "),
                p("Shewhart charts can help understand whether and how a measure is changing
                  over time. The basic components of a Shewhart chart are:"),
                tagAppendAttributes(tags$ol(
                  tags$li("A measure plotted over time as a line graph"),
                  tags$li("A horizontal centre line, often an average, indicating the typical level of the measure"),
                  tags$li("Control limits: horizontal lines indicating the range the measure usually stays within")
                ), type = 'A'),
                p("To interpret a Shewhart chart, a set of rules is used. The rules indicate when the way the
                  measure varies is different in some way from its typical behaviour. The rules we use in this
                  analysis are as follows:"),
                tags$ol(
                  tags$li("Any month outside the control limits (month highlighted in orange)"),
                  tags$li("Eight or more consecutive months all above, or all below, the centre line
                          (months highlighted in blue)")
                ),
                p("Any instance of one of these rules being triggered by a measure indicates that there is likely
                  to be an identifiable cause for this particular pattern in the data. This is known as a special cause
                  and is worth investigating locally. Learning from special cause variation in a measure can indicate actions
                  that will result in improvements in patient care in future."),
                p("If no rules are triggered, this means that the measure is continuing to behave as it usually does. This
                  indicates that in order to achieve improvements, there is no point in focussing on individual months' highs
                  and lows. The system needs to be locally diagnosed, and improvments implemented based on the results."),
                h2("Resources"),
                p("An introduction to control charts in healthcare:"),
                a("Statistical process control as a tool for research and healthcare improvement",
                  href="http://dx.doi.org/10.1136/qhc.12.6.458"),
                br(),
                p("Practical, interactive guide to using data to drive improvement:"),
                a("NHS Improvement: Making Data Count", href="https://improvement.nhs.uk/resources/making-data-count/"),
                br(),
                p("This analysis uses p-prime and u-prime charts, more information
                  is available here:"),
                a("Prime charts publication", href="http://dx.doi.org/10.1136/qshc.2006.017830")
        ),
       tabItem(tabName = "dev",
               h1("Development"),
               p("This website was developed by the NIHR CLAHRC Northwest London
                 Public Health and Information Intelligence Theme in collaboration with
                  NHS England London. It is in a beta-testing phase at present, meaning
                 that we will review and make further improvements to the site on a regular
                 basis."),
               h2("Coming soon..."),
               p("In the near future we hope to implement the following new features:"),
               tags$ol(
                 tags$li("Regional and national aggregated analysis"),
                 tags$li("Download buttons to make it easy to save the plots (right click and save as for now)"),
                 tags$li("Improved look and feel"),
                 tags$li("Distinct periods for control limits, to better reflect shifts in the measures")
               )
               )
      )
    )
)


# Define server logic
server <- function(input, output) {
  
  # If new data has been released since the app was launched,
  # download it
  if(update_data) {
    current_data_urls <- getAEdata_urls_monthly()
  } else {
      current_data_urls <- NULL
  }
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
  provLookup <- provLookup %>% arrange(Prov_Name) %>%
    add_row(Prov_Name = "Region: London", Prov_Code = "L") %>%
    add_row(Prov_Name = "Region: Midlands", Prov_Code = "M") %>%
    add_row(Prov_Name = "Region: North of England", Prov_Code = "N") %>%
    add_row(Prov_Name = "Region: South of England", Prov_Code = "S") %>%
    add_row(Prov_Name = "Whole of England", Prov_Code = "E")
    
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

  edPerfPlotInput <- function() {
    if (length(input$trust) != 0) {
      pr <- c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Code'][1,1])
      measure <- "All"
      if(input$t1_only_checkbox) {measure <- "Typ1"} 
      tryCatch(plot_performance(AE_Data, prov_codes = pr, start.date = perf.start.date, end.date = perf.end.date,
                                brk.date = perf.brk.date, date.col = 'Month_Start',
                                x_title = "Month", measure = measure,
                                r1_col = r1_col, r2_col=r2_col), 
               error=function(e) NULL)
    }
  }
  
  output$edPerfPlot <- renderPlot({
    print(edPerfPlotInput())
  })
  
  edVolPlotInput <- function() {
    if (length(input$trust) != 0) {
      pr <- c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Code'][1,1])
      measure <- "All"
      if(input$t1_only_checkbox) {measure <- "Typ1"}
      tryCatch(plot_volume(AE_Data, prov_codes = pr, start.date = perf.start.date, end.date = perf.end.date,
                           brk.date = perf.brk.date, date.col = 'Month_Start',
                           x_title = "Month", measure = measure,
                           r1_col = r1_col, r2_col=r2_col), 
               error=function(e) NULL)
    }
  }
  
  output$edVolPlot <- renderPlot({
    print(edVolPlotInput())
  })
  
  # R studio bug so correct download name only works when you run app via runApp(launch.browser = T) command
  output$downloadPerfPlot <- downloadHandler( 
    filename = function() {
      paste(gsub(" ","_",gsub(" NHS |Foundation |Trust",'',c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Name']))),
            "_PerfPlot_",ifelse(input$t1_only_checkbox,"Type1","AllTypes"),"_",
            perf.start.date,"/",perf.end.date,".png", sep = "")
    },
    content = function(file){
      png(file, width = 10, height = 5.5, units = 'in', res = 300) 
      print(edPerfPlotInput())
      dev.off()
    })
  
  output$downloadVolPlot <- downloadHandler(
    filename = function() {
      paste(gsub(" ","_",gsub(" NHS |Foundation |Trust",'',c(provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Name']))),
            "_AttendPlot_",ifelse(input$t1_only_checkbox,"Type1","AllTypes"),"_",
            perf.start.date,"/",perf.end.date,".png", sep = "")
    },
    content = function(file) {
      png(file, width = 10, height = 5, units = 'in', res = 300) 
      print(edVolPlotInput())
      dev.off()
    })

}


# Run the application 
shinyApp(ui = ui, server = server)
