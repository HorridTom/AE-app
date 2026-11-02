app_server <- function(
  ae_data,
  organisations,
  start_date = "2015-07-01",
  end_date = Sys.Date()
) {
  function(input, output, session) {
    # The analysis level labels depend on the country. Changing country starts
    # again at national level.
    observeEvent(input$country, ignoreInit = TRUE, {
      updateRadioButtons(
        session,
        "level",
        choiceNames = level_labels(input$country),
        choiceValues = c("National", "Regional", "Provider"),
        selected = "National"
      )
    })

    output$subtitle <- renderText({
      req(input$country)
      paste("NHS", input$country, "Provider Organisations")
    })

    # A provider's earlier names, if it has had any (issue #32)
    output$earlier_names <- renderText({
      s <- selection()
      req(s$level == "Provider")
      earlier_names_note(ae_data, s$code)
    })

    selection <- reactive({
      req(input$country, input$level)
      make_selection(input, organisations)
    })

    series <- reactive({
      s <- selection()
      make_series(
        ae_data,
        s$level,
        s$code,
        s$measure,
        s$weekly,
        s$only_reporting
      )
    })

    # NULL if the chart can't be drawn, e.g. no data for the selection. A
    # req() that isn't met (e.g. while the inputs are first set up) still
    # stops the chart quietly.
    try_chart <- function(f, ...) {
      s <- selection()
      tryCatch(
        f(
          series(),
          s$level,
          s$measure,
          s$only_reporting,
          s$weekly,
          ...,
          start_date = start_date,
          end_date = end_date
        ),
        shiny.silent.error = function(e) stop(e),
        error = function(e) NULL
      )
    }

    file_name <- function(chart_name) {
      reactive(download_file_name(
        selection(),
        chart_name,
        start_date,
        end_date
      ))
    }

    chart_server(
      "performance",
      reactive(try_chart(chart_performance)),
      file_name("PerfPlot"),
      height = 5.5
    )
    chart_server(
      "attendances",
      reactive(try_chart(chart_volume, volume = "Attendances")),
      file_name("AttendPlot")
    )
    chart_server(
      "admissions",
      reactive({
        req(input$country == "England")
        try_chart(chart_volume, volume = "Admissions")
      }),
      file_name("AdmissPlot")
    )
  }
}


# What the user has chosen, from the inputs. Options apply only to the
# country they belong to: their inputs keep their values while hidden.
make_selection <- function(input, organisations) {
  country <- input$country
  level <- input$level
  england <- country == "England"

  if (level == "Provider") {
    # The provider menu's values are provider codes
    code <- if (england) input$trust else input$trustScot
    req(code)
    name <- provider_name(organisations, code)
  } else {
    name <- if (level == "Regional") {
      if (england) input$region else input$regionScot
    } else {
      country
    }
    req(name)
    code <- organisation_code(organisations, country, level, name)
  }

  list(
    country = country,
    level = level,
    name = name,
    code = code,
    measure = if (england && isTRUE(input$t1_only_checkbox)) "Typ1" else "All",
    weekly = !england && isTRUE(input$weekly_checkbox),
    only_reporting = england && isTRUE(input$still_reporting_checkbox)
  )
}


# e.g. "Chelsea_And_Westminster_Hospital_PerfPlot_AllTypes_2015-07-01_to_2026-11-02.png"
download_file_name <- function(selection, chart_name, start_date, end_date) {
  type <- if (selection$country != "England") {
    ""
  } else if (selection$measure == "Typ1") {
    "Type1"
  } else {
    "AllTypes"
  }
  name <- gsub(" ", "_", gsub(" NHS |Foundation |Trust", "", selection$name))
  paste0(
    name,
    "_",
    chart_name,
    "_",
    type,
    "_",
    start_date,
    "_to_",
    end_date,
    ".png"
  )
}
