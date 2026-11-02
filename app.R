library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)

Sys.setenv(TZ='Europe/London')
source("spc_rules.R")
source("perf_4h_analysis.R")
source("data_loading.R")

#suppress summarise informating printing
options(dplyr.summarise.inform = FALSE)

r1_col = "orange"
r2_col = "steelblue3"

AE_Data <- read_ae_file("England")
AE_Data_Scot <- read_ae_file("Scotland")

AE_Data <- clean_region_col(AE_Data)
AE_Data_Scot <- standardise_data(AE_Data_Scot)

AE_Data <- dplyr::bind_rows(AE_Data, AE_Data_Scot)

# Define UI
ui <- dashboardPage(

   # Application title
   dashboardHeader(title = "A&E Tracker",
                    titleWidth = 300
    ),
   dashboardSidebar(tags$style(".left-side, .main-sidebar {padding-top: 50px}"),
                    width = 300,
                    tags$a(href='http://clahrc-northwestlondon.nihr.ac.uk/',
                           tags$img(src="CLAHRC-logo-white.png", height = 60, width = 190, vspace="10"),
                           target="_blank"),
      sidebarMenu(id = "tabs", menuItem("Analyse A&E data", tabName = "analysis", icon = icon("hospital"))
      ),
      conditionalPanel(condition = "input.tabs === 'analysis'",
                       uiOutput("countryChoice"),
                       uiOutput("radioBut")
      ),
      conditionalPanel(condition = "input.tabs === 'analysis' & input.country == 'England'",
                       uiOutput("typ")
      ),
      conditionalPanel(condition = "input.tabs === 'analysis' & input.level != 'Provider' & input.country == 'England'",
                       uiOutput("stillReporting")
      ),
      conditionalPanel(condition = "input.tabs === 'analysis' & input.country == 'Scotland'",
                       uiOutput("weekOrMonth")
      ),
      conditionalPanel(condition = "input.tabs === 'analysis' & input.level == 'Provider' & input.country == 'England'" ,
                       uiOutput("orgChoice")
      ),
      conditionalPanel(condition = "input.tabs === 'analysis' & input.level == 'Provider' & input.country == 'Scotland'" ,
                       uiOutput("orgChoiceScot")
      ),
      conditionalPanel(condition = "input.tabs === 'analysis' & input.level == 'Regional' & input.country == 'England'",
                       uiOutput("regChoice")
      ),
      conditionalPanel(condition = "input.tabs === 'analysis' & input.level == 'Regional' & input.country == 'Scotland'",
                       uiOutput("regChoiceScot")
      ),
      sidebarMenu(id = "tabs",
                  menuItem("Understanding the analysis", tabName = "understanding", icon = icon('info-circle')),
                  menuItem("Development", tabName = "dev", icon = icon('road'))
      )
    ),

   dashboardBody(shinyjs::useShinyjs(),
    # No CSS styling for now
    #tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #),
     tabItems(
       tabItem(tabName = "analysis",
                h1("Analysis of Accident and Emergency Attendance Data"),
                #h4("NHS England Provider Organisations"), ###needs to be updated for Scotland
                h4(uiOutput("subtitle")),
                fluidRow( 
                         column(width = 12,
                                box(plotOutput("edPerfPlot"), width = NULL),
                                box(plotOutput("edVolPlot"), width = NULL),
                                box(id = "admissionsPlot", plotOutput("edAdmVolPlot"), width = NULL)
                                )
                         ),
               downloadButton('downloadPerfPlot', label = 'Download Performance Chart'), 
               downloadButton('downloadVolPlot', label = 'Download Attendances Chart'),
               conditionalPanel(condition = "input.tabs === 'analysis' & input.country == 'England'",
                                uiOutput("downButt")
               )
        ),
        tabItem(tabName = "understanding",
                h1("Understanding the analysis"),
                p("This application provides statistical analysis of attendance and admission data relating
                  to providers of NHS accident and emergency department services in England and Scotland."),
                p("All the data used
                is publicly available from the NHS England and ISD Scotland websites:"),
                a("A&E waiting times and activity for England",
                  href="https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/"),
                p("\n"),
                a("A&E waiting times and activity for Scotland",
                  href="https://www.opendata.nhs.scot/dataset/weekly-accident-and-emergency-activity-and-waiting-times"),
                p("\n"),
                p("Contains public sector information licensed under the ",
                  a("Open Government Licence v3.0",
                    href = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"),
                  ". This analysis is not an official publication of NHS England or Public Health Scotland."),
                p("\n"),
                p("Note that due to the 'rapid care measures' pilot, data for some NHS England providers is not available from May 2019.
                  By default this analysis now excludes these providers for the whole time period. If you wish to see the analysis
                  for all providers, untick the control box on the right hand panel. The four hour performance measure is not comparable
                  before and after the start of the pilot in this case, and so is only displayed up to April 2019." ),
                p("\n"),
                a("King's Fund report on the review of A&E standards",
                  href="https://www.kingsfund.org.uk/publications/nhs-england-review-waiting-times-accident-emergency"),
                p("\n"),
                p("Note that NHS Scotland data is provided weekly, and may be analysed either weekly as-is, or attributed
                  across months and analysed monthly. Tick/untick the 'Weekly analysis' box to toggle between these
                  two analysis modes."),
                p("\n"),
                p("Note that Scottish data for admissions is not currently publically available and therefore admissions charts are not included in the analysis for Scotland."),
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
                  tags$li("Any month/week outside the control limits (highlighted in orange)"),
                  tags$li("Eight or more consecutive months/weeks all above, or all below, the centre line
                          (highlighted in blue)")
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
               p("The code for this site is under an open source licence and is available here:"),
               a("A&E Tracker code repository", href="https://github.com/HorridTom/AE-app"),
               h2("Coming soon..."),
               p("In the near future we hope to implement the following new features:"),
               tags$ol(
                 tags$li("Improved look and feel"),
                 tags$li("Distinct periods for control limits, to better reflect shifts in the measures")
               )
               )
      )
    )
)


# Define server logic
server <- function(input, output) {
  
  #currently no admissions data for Scotland so hides admissions graph
  observeEvent(input$country, {
    if(input$country == "England"){
      shinyjs::show(id = "admissionsPlot")
    }else{
      shinyjs::hide(id = "admissionsPlot")
    }
  })
  
  provLookup <- AE_Data[!duplicated(AE_Data[,c('Prov_Code')]),c('Prov_Code','Prov_Name','Reg_Code','Region','Nat_Code','Country')]
  provLookup <- provLookup %>% arrange(Prov_Name) 
  
  orgNames <- provLookup[which(provLookup$Country == "England"),'Prov_Name']
  orgNamesScot <- provLookup[which(provLookup$Country == "Scotland"),'Prov_Name']
  regNames <- levels(factor(provLookup[which(provLookup$Country == "England"),'Region']))
  regNamesScot <- levels(factor(provLookup[which(provLookup$Country == "Scotland"),'Region']))
  couNames <- levels(factor(provLookup$Country))
  
  regLab <- reactive({ifelse(input$country == "England", "Regional","Board Level")})
  orgLab <- reactive({ifelse(input$country == "England", "Provider Level","Hospital Level")})
  
  perf.start.date <- "2015-07-01"
  perf.end.date <- lubridate::today()
  perf.brk.date <- NULL
  
  #reactive UI inputs
  output$subtitle <- renderUI({ifelse(input$country == "England", "NHS England Provider Organisations", "NHS Scotland Provider Organisations")})
  output$countryChoice <- renderUI({selectInput("country", "Choose Country", couNames)})
  output$radioBut <- renderUI({
    if(length(input$country != 0)){
      radioButtons("level", "Select Analysis Level",
                                  choiceValues = c("National", "Regional", "Provider"),
                                  choiceNames = c("National", regLab(), orgLab()))
      }
    })
  
  output$stillReporting <- renderUI({checkboxInput("still_reporting_checkbox", label = "Exclude providers with missing data", value = TRUE)})
  output$typ <- renderUI({checkboxInput("t1_only_checkbox", label = "Only include type 1 departments", value = FALSE)})
  output$weekOrMonth <- renderUI({checkboxInput("weekly_checkbox", label = "Weekly analysis", value = FALSE)})
  output$orgChoice <- renderUI({selectInput("trust", "Choose Provider", orgNames)})
  output$orgChoiceScot <- renderUI({selectInput("trustScot", "Choose Provider", orgNamesScot)})
  output$regChoice <- renderUI({selectInput("region", "Choose Region", regNames)})
  output$regChoiceScot <- renderUI({selectInput("regionScot", "Choose Board", regNamesScot)})
  output$menuItemAn <- renderUI({menuItem("Understanding the analysis", tabName = "understanding", icon = icon('info-circle'))})
  output$menuItemDev <- renderUI({menuItem("Development", tabName = "dev", icon = icon('road'))})
  output$downButt <- renderUI({downloadButton('downloadAdmVolPlot', 'Download Admissions Chart')})
  

  # The code of the organisation (provider, region/board or country) chosen
  selected_code <- reactive({
    req(input$country, input$level)
    lookup <- provLookup[provLookup$Country == input$country, ]
    england <- input$country == "England"
    if (input$level == "Provider") {
      name <- if (england) input$trust else input$trustScot
      req(name)
      lookup$Prov_Code[lookup$Prov_Name == name][1]
    } else if (input$level == "Regional") {
      name <- if (england) input$region else input$regionScot
      req(name)
      lookup$Reg_Code[lookup$Region == name][1]
    } else {
      lookup$Nat_Code[1]
    }
  })

  edPerfPlotInput <- function() {
    {
      level <- input$level
      code <- selected_code()
      
      measure <- "All"
      weeklyOrMonthly <- "Monthly"
      onlyProvsReporting <- F
      if(input$country == "England" && isTRUE(input$t1_only_checkbox)) {measure <- "Typ1"} 
      if(input$country == "Scotland" && isTRUE(input$weekly_checkbox)) {weeklyOrMonthly <- "weekly"}
      if(input$country == "England" && isTRUE(input$still_reporting_checkbox)) {onlyProvsReporting <- T}
      tryCatch(plot_performance(AE_Data, code = code, start.date = perf.start.date, end.date = perf.end.date,
                                brk.date = perf.brk.date, date.col = 'Month_Start',
                                x_title = "Month", measure = measure,
                                r1_col = r1_col, r2_col=r2_col,
                                level = level, weeklyOrMonthly = weeklyOrMonthly,
                                onlyProvsReporting = onlyProvsReporting), 
               error=function(e) NULL)
    }
  }
  
  output$edPerfPlot <- renderPlot({
    req(input$country, input$level)
    chart <- edPerfPlotInput()
    validate(need(!is.null(chart), "There is no data to chart for this selection."))
    print(chart)
  })
  
  edVolPlotInput <- function(volumeType = "Attendances") {
    {
      level <- input$level
      code <- selected_code()
      
      measure <- "All"
      onlyProvsReporting <- F
      if(input$country == "England" && isTRUE(input$t1_only_checkbox)) {measure <- "Typ1"}
      weeklyOrMonthly <- "Monthly"
      if(input$country == "Scotland" && isTRUE(input$weekly_checkbox)) {weeklyOrMonthly <- "weekly"}
      if(input$country == "England" && isTRUE(input$still_reporting_checkbox)) {onlyProvsReporting <- T}
      
      if(volumeType == "Attendances"){
        tryCatch(plot_volume(AE_Data, code = code, start.date = perf.start.date, end.date = perf.end.date,
                             brk.date = perf.brk.date, date.col = 'Month_Start',
                             x_title = "Month", measure = measure,
                             r1_col = r1_col, r2_col=r2_col,
                             level = level, weeklyOrMonthly = weeklyOrMonthly,
                             onlyProvsReporting = onlyProvsReporting,
                             attOrAdm = "Attendances"), 
                 error=function(e) NULL)
      }else{
        tryCatch(plot_volume(AE_Data, code = code, start.date = perf.start.date, end.date = perf.end.date,
                             brk.date = perf.brk.date, date.col = 'Month_Start',
                             x_title = "Month", measure = measure,
                             r1_col = r1_col, r2_col=r2_col,
                             level = level, weeklyOrMonthly = weeklyOrMonthly,
                             onlyProvsReporting = onlyProvsReporting,
                             attOrAdm = "Admissions"), 
                 error=function(e) NULL)
      }
      
    }
  }
  
  output$edVolPlot <- renderPlot({
    req(input$country, input$level)
    chart <- edVolPlotInput(volumeType = "Attendances")
    validate(need(!is.null(chart), "There is no data to chart for this selection."))
    print(chart)
  })
  
  output$edAdmVolPlot <- renderPlot({
    req(input$country == "England")
    chart <- edVolPlotInput(volumeType = "Admissions")
    validate(need(!is.null(chart), "There is no data to chart for this selection."))
    print(chart)
  })
  
  # R studio bug so correct download name only works when you run app via runApp(launch.browser = T) command
  output$downloadPerfPlot <- downloadHandler( 
    filename = function(plot = "Performance") {
      if(input$country == "England"){
        name <- ifelse(input$level == "Provider", provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Name'],
                       ifelse(input$level == "Regional", provLookup[which(provLookup$Region == input$region),'Region'], 
                              provLookup[which(provLookup$Country == input$country),'Country']))
        typ <- ifelse(isTRUE(input$t1_only_checkbox),"Type1","AllTypes")
        
      }else{
        name <- ifelse(input$level == "Provider", provLookup[which(provLookup$Prov_Name == input$trustScot),'Prov_Name'],
                       ifelse(input$level == "Regional", provLookup[which(provLookup$Region == input$regionScot),'Region'], 
                              provLookup[which(provLookup$Country == input$country),'Country']))
        typ <- ""
      }
      
      plotTypeName <- ifelse(plot == "Performance", "_PerfPlot_", ifelse(plot == "Attendances", "_AttendPlot_", "_AdmissPlot_"))
      
      paste(gsub(" ","_",gsub(" NHS |Foundation |Trust",'',name)),
            plotTypeName,typ,"_",
            perf.start.date,"_to_",perf.end.date,".png", sep = "")
    },
    content = function(file){
      png(file, width = 10, height = 5.5, units = 'in', res = 300) 
      print(edPerfPlotInput())
      dev.off()
    })
  
  output$downloadVolPlot <- downloadHandler(
    filename = function(plot = "Attendances") {
      if(input$country == "England"){
        name <- ifelse(input$level == "Provider", provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Name'],
                       ifelse(input$level == "Regional", provLookup[which(provLookup$Region == input$region),'Region'], 
                              provLookup[which(provLookup$Country == input$country),'Country']))
        typ <- ifelse(isTRUE(input$t1_only_checkbox),"Type1","AllTypes")
        
      }else{
        name <- ifelse(input$level == "Provider", provLookup[which(provLookup$Prov_Name == input$trustScot),'Prov_Name'],
                       ifelse(input$level == "Regional", provLookup[which(provLookup$Region == input$regionScot),'Region'], 
                              provLookup[which(provLookup$Country == input$country),'Country']))
        typ <- ""
      }
      
      plotTypeName <- ifelse(plot == "Performance", "_PerfPlot_", ifelse(plot == "Attendances", "_AttendPlot_", "_AdmissPlot_"))
      
      paste(gsub(" ","_",gsub(" NHS |Foundation |Trust",'',name)),
            plotTypeName,typ,"_",
            perf.start.date,"_to_",perf.end.date,".png", sep = "")
    },
    content = function(file) {
      png(file, width = 10, height = 5, units = 'in', res = 300) 
      print(edVolPlotInput(volumeType = "Attendances"))
      dev.off()
    })
  
  output$downloadAdmVolPlot <- downloadHandler(
    filename = function(plot = "Admissions") {
      if(input$country == "England"){
        name <- ifelse(input$level == "Provider", provLookup[which(provLookup$Prov_Name == input$trust),'Prov_Name'],
                       ifelse(input$level == "Regional", provLookup[which(provLookup$Region == input$region),'Region'], 
                              provLookup[which(provLookup$Country == input$country),'Country']))
        typ <- ifelse(isTRUE(input$t1_only_checkbox),"Type1","AllTypes")
        
      }else{
        name <- ifelse(input$level == "Provider", provLookup[which(provLookup$Prov_Name == input$trustScot),'Prov_Name'],
                       ifelse(input$level == "Regional", provLookup[which(provLookup$Region == input$regionScot),'Region'], 
                              provLookup[which(provLookup$Country == input$country),'Country']))
        typ <- ""
      }
      
      plotTypeName <- ifelse(plot == "Performance", "_PerfPlot_", ifelse(plot == "Attendances", "_AttendPlot_", "_AdmissPlot_"))
      
      paste(gsub(" ","_",gsub(" NHS |Foundation |Trust",'',name)),
            plotTypeName,typ,"_",
            perf.start.date,"_to_",perf.end.date,".png", sep = "")
    },
    content = function(file) {
      png(file, width = 10, height = 5, units = 'in', res = 300) 
      print(edVolPlotInput(volumeType = "Admissions"))
      dev.off()
    })

}


# Run the application 
shinyApp(ui = ui, server = server)
