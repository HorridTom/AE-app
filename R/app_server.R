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

    # Notes shown above the charts: a provider's earlier names, and links to
    # the codes it was reported under before or after a change of code (issue
    # #32); and why a performance chart has no control limits, where every
    # attendance was within 4 hours
    output$chart_notes <- renderUI({
      s <- selection()
      notes <- list(
        if (s$level == "Provider") {
          provider_notes(
            earlier_names_note(ae_data, s$code),
            code_handovers(ae_data, organisations, s$code)
          )
        },
        note_paragraph(tryCatch(
          all_within_4h_note(series(), s$weekly, start_date, end_date),
          error = function(e) ""
        ))
      )
      tagList(Filter(Negate(is.null), notes))
    })

    # Following one of those links
    observeEvent(input$switch_provider, {
      menu <- if (input$country == "England") "trust" else "trustScot"
      updateSelectInput(session, menu, selected = input$switch_provider)
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

    # The chart, or if it can't be drawn, a condition whose message explains
    # why (see no_chart()). A req() that isn't met (e.g. while the inputs are
    # first set up) still stops the chart quietly.
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
          end_date = end_date,
          show_from = shown_dates()[1],
          show_to = shown_dates()[2],
          x_title = if (s$weekly) "Week" else "Month"
        ),
        shiny.silent.error = function(e) stop(e),
        no_chart = function(e) e,
        error = function(e) {
          warning("Chart failed: ", conditionMessage(e), call. = FALSE)
          simpleCondition("There is no data to chart for this selection.")
        }
      )
    }

    # The dates the user has chosen to show. The charts are analysed on all
    # the data from start_date to end_date and then zoomed to these dates, so
    # that the limits at any point do not depend on the dates shown. Debounced
    # so that dragging the slider doesn't redraw the charts at every step.
    shown_dates <- debounce(
      reactive({
        req(input$dates)
        as.Date(input$dates)
      }),
      500
    )

    caption <- function(chart_type) {
      reactive(chart_caption(chart_type, selection()$country))
    }

    file_name <- function(chart_name) {
      reactive(download_file_name(
        selection(),
        chart_name,
        shown_dates()[1],
        shown_dates()[2]
      ))
    }

    chart_server(
      "performance",
      reactive(try_chart(chart_performance)),
      file_name("PerfPlot"),
      caption("P'"),
      height = 6.25
    )
    chart_server(
      "attendances",
      reactive(try_chart(chart_volume, volume = "Attendances")),
      file_name("AttendPlot"),
      caption("U'")
    )
    chart_server(
      "admissions",
      reactive({
        req(input$country == "England")
        try_chart(chart_volume, volume = "Admissions")
      }),
      file_name("AdmissPlot"),
      caption("U'")
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


# The notes shown above a provider's charts: its earlier names, and links to
# the codes it was reported under before or after a change of code
provider_notes <- function(earlier_names, handovers) {
  month <- function(date) format(date, "%B %Y")
  link_to <- function(code) {
    tags$a(
      href = "#",
      onclick = sprintf(
        "Shiny.setInputValue('switch_provider', '%s', {priority: 'event'}); return false;",
        code
      ),
      code
    )
  }

  previous <- handovers$previous
  following <- handovers[["next"]]
  notes <- list(
    if (nzchar(earlier_names)) earlier_names,
    if (!is.null(previous)) {
      HTML(paste0(
        "Previously reported under code ",
        link_to(previous$Prov_Code),
        ", up to ",
        month(previous$Last),
        "."
      ))
    },
    if (!is.null(following)) {
      HTML(paste0(
        "Reported under code ",
        link_to(following$Prov_Code),
        " from ",
        month(following$First),
        "."
      ))
    }
  )
  notes <- Filter(Negate(is.null), notes)
  if (length(notes) == 0) {
    return(NULL)
  }
  tagList(lapply(notes, p))
}


note_paragraph <- function(note) {
  if (nzchar(note)) p(note)
}
