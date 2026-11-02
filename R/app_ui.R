app_ui <- function(organisations) {
  dashboardPage(
    dashboardHeader(title = "A&E Tracker", titleWidth = 300),
    app_sidebar(organisations),
    dashboardBody(
      tabItems(
        tabItem(tabName = "analysis", analysis_tab()),
        tabItem(tabName = "understanding", understanding_tab_content()),
        tabItem(tabName = "dev", development_tab_content())
      )
    )
  )
}

# In conditionalPanel() conditions
js_england <- "input.country == 'England'"
js_scotland <- "input.country == 'Scotland'"

app_sidebar <- function(organisations) {
  in_analysis <- function(condition) {
    paste("input.tabs === 'analysis' &", condition)
  }

  dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 50px}"),
    width = 300,
    tags$a(
      href = "http://clahrc-northwestlondon.nihr.ac.uk/",
      tags$img(
        src = "CLAHRC-logo-white.png",
        height = 60,
        width = 190,
        vspace = "10"
      ),
      target = "_blank"
    ),
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Analyse A&E data",
        tabName = "analysis",
        icon = icon("hospital")
      )
    ),
    conditionalPanel(
      condition = "input.tabs === 'analysis'",
      selectInput("country", "Choose Country", country_names(organisations)),
      radioButtons(
        "level",
        "Select Analysis Level",
        choiceNames = level_labels("England"),
        choiceValues = c("National", "Regional", "Provider")
      )
    ),
    conditionalPanel(
      condition = in_analysis(js_england),
      checkboxInput(
        "t1_only_checkbox",
        label = "Only include type 1 departments",
        value = FALSE
      )
    ),
    conditionalPanel(
      condition = in_analysis(paste("input.level != 'Provider' &", js_england)),
      checkboxInput(
        "still_reporting_checkbox",
        label = "Exclude providers with missing data",
        value = TRUE
      )
    ),
    conditionalPanel(
      condition = in_analysis(js_scotland),
      checkboxInput("weekly_checkbox", label = "Weekly analysis", value = FALSE)
    ),
    conditionalPanel(
      condition = in_analysis(paste("input.level == 'Provider' &", js_england)),
      selectInput(
        "trust",
        "Choose Provider",
        provider_choices(organisations, "England")
      )
    ),
    conditionalPanel(
      condition = in_analysis(paste(
        "input.level == 'Provider' &",
        js_scotland
      )),
      selectInput(
        "trustScot",
        "Choose Provider",
        provider_choices(organisations, "Scotland")
      )
    ),
    conditionalPanel(
      condition = in_analysis(paste("input.level == 'Regional' &", js_england)),
      selectInput(
        "region",
        "Choose Region",
        region_names(organisations, "England")
      )
    ),
    conditionalPanel(
      condition = in_analysis(paste(
        "input.level == 'Regional' &",
        js_scotland
      )),
      selectInput(
        "regionScot",
        "Choose Board",
        region_names(organisations, "Scotland")
      )
    ),
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Understanding the analysis",
        tabName = "understanding",
        icon = icon("info-circle")
      ),
      menuItem("Development", tabName = "dev", icon = icon("road"))
    )
  )
}

# Labels for the analysis levels, which differ between countries
level_labels <- function(country) {
  if (country == "England") {
    c("National", "Regional", "Provider Level")
  } else {
    c("National", "Board Level", "Hospital Level")
  }
}

analysis_tab <- function() {
  tagList(
    h1("Analysis of Accident and Emergency Attendance Data"),
    h4(textOutput("subtitle")),
    p(textOutput("earlier_names", inline = TRUE)),
    fluidRow(
      column(
        width = 12,
        box(chart_plot_ui("performance"), width = NULL),
        box(chart_plot_ui("attendances"), width = NULL),
        # There is no admissions data for Scotland
        conditionalPanel(
          condition = js_england,
          box(chart_plot_ui("admissions"), width = NULL)
        )
      )
    ),
    chart_download_ui("performance", "Download Performance Chart"),
    chart_download_ui("attendances", "Download Attendances Chart"),
    conditionalPanel(
      condition = paste("input.tabs === 'analysis' &", js_england),
      chart_download_ui("admissions", "Download Admissions Chart")
    )
  )
}
