# start_date and end_date: the dates analysed, and the widest range the user
# can choose to show
app_ui <- function(
  organisations,
  start_date = "2015-07-01",
  end_date = Sys.Date()
) {
  dashboardPage(
    dashboardHeader(title = "A&E Tracker", titleWidth = 300),
    app_sidebar(organisations, start_date, end_date),
    dashboardBody(
      useBusyIndicators(),
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

app_sidebar <- function(organisations, start_date, end_date) {
  in_analysis <- function(condition) {
    paste("input.tabs === 'analysis' &", condition)
  }

  dashboardSidebar(
    tags$style(HTML(
      ".left-side, .main-sidebar {padding-top: 50px}",
      # On small screens the header is two rows high, and the main heading
      # is smaller
      "@media (max-width: 767px) {",
      "  .left-side, .main-sidebar {padding-top: 100px}",
      "  .content-wrapper h1 {font-size: 24px}",
      "}"
    )),
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
    conditionalPanel(
      condition = "input.tabs === 'analysis'",
      sliderInput(
        "dates",
        "Show dates",
        min = as.Date(start_date),
        max = as.Date(end_date),
        value = c(as.Date(start_date), as.Date(end_date)),
        timeFormat = "%b %Y"
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
    uiOutput("chart_notes"),
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
    charts_note(),
    chart_download_ui("performance", "Download Performance Chart"),
    chart_download_ui("attendances", "Download Attendances Chart"),
    conditionalPanel(
      condition = paste("input.tabs === 'analysis' &", js_england),
      chart_download_ui("admissions", "Download Admissions Chart")
    )
  )
}


# One note for all the charts, in place of a caption on each
charts_note <- function() {
  tags$div(
    class = "charts-note",
    style = "margin: 0 0 15px; font-size: 90%;",
    p(
      "These are Shewhart charts: a P\u2032 chart of the percentage of",
      "attendances within 4 hours, and U\u2032 charts of the average daily",
      "numbers of attendances and admissions. Rule 1: any point outside the",
      "control limits. Rule 2: eight or more consecutive points all above, or",
      "all below, the centre line. Points excluded from the calculation of the",
      "limits are grey. The attendances and admissions charts start at",
      "zero; where the performance chart does not, a break is marked at the",
      "foot of its vertical axis. See \"Understanding the analysis\" for",
      "more."
    ),
    conditionalPanel(
      condition = paste(js_scotland, "& !input.weekly_checkbox"),
      p(
        "Public Health Scotland publishes weekly figures. For this monthly",
        "analysis, each week's figures are shared between the months its days",
        "fall in, and the first and last months, which the weeks only partly",
        "cover, are left out. Tick \"Weekly analysis\" to see the weekly",
        "figures as published."
      )
    )
  )
}
