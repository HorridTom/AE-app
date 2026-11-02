# Shewhart charts of a series made by make_series(), drawn with autospc.

#' Chart of the percentage of attendances within 4 hours (P' chart)
#'
#' @param series a data frame from make_series()
#' @param level,measure,only_reporting,weekly the values used to make the
#'   series, for the chart's subtitle and title
#' @param start_date,end_date the period to chart
#' @param x_title horizontal axis title
chart_performance <- function(
  series,
  level,
  measure = "All",
  only_reporting = FALSE,
  weekly = FALSE,
  start_date = "2015-07-01",
  end_date = Sys.Date(),
  x_title = "Month"
) {
  series <- series |> filter(!is.na(Within_4h))
  name <- latest_name(series)
  series <- restrict_to_period(series, start_date, end_date)

  # autospc needs whole-number counts. Counts are fractional where weekly
  # data have been apportioned to months (Scotland), and in a few source
  # rows; round them, as autospc would otherwise do with a warning.
  series <- series |>
    mutate(Within_4h = round(Within_4h), Total_Att = round(Total_Att))

  autospc::autospc(
    series,
    x = Period,
    y = Within_4h,
    n = Total_Att,
    chart_type = "P'",
    max_exclusions = 3L,
    title = "Percentage A&E attendances\nwith time in department < 4h",
    subtitle = chart_subtitle(series, name, level, measure, only_reporting),
    override_x_title = x_title,
    override_y_title = "Percentage within 4 hours",
    annotation_size = 4,
    upper_annotation_sf = 1.08,
    lower_annotation_sf = 0.92,
    flip_labels = TRUE
  )
}


#' Chart of average daily attendances or admissions (U' chart)
#'
#' The U' chart plots each period's total divided by its number of days, so
#' that months of different lengths, and weeks, are on the same scale.
#'
#' @inheritParams chart_performance
#' @param volume "Attendances" or "Admissions"
chart_volume <- function(
  series,
  level,
  measure = "All",
  only_reporting = FALSE,
  weekly = FALSE,
  volume = "Attendances",
  start_date = "2015-07-01",
  end_date = Sys.Date(),
  x_title = "Month"
) {
  # The column of `series` holding the chosen volume
  total_col <- c(Attendances = "Total_Att", Admissions = "Total_Adm")[[volume]]

  name <- latest_name(series)
  series <- restrict_to_period(series, start_date, end_date)

  # e.g. admissions for a provider that has none: nothing to chart
  if (all(is.na(series[[total_col]]) | series[[total_col]] == 0)) {
    stop("No ", tolower(volume), " data for this selection")
  }

  # autospc needs whole-number counts. Counts are fractional where weekly
  # data have been apportioned to months (Scotland), and in a few source
  # rows; round them, as autospc would otherwise do with a warning.
  series$total <- round(series[[total_col]])

  per <- if (weekly) "week" else "month"
  title <- switch(
    volume,
    Attendances = paste("Average daily A&E attendances per", per),
    Admissions = paste("Average daily admissions from A&E per", per)
  )

  autospc::autospc(
    series,
    x = Period,
    y = total,
    n = days_in_period,
    chart_type = "U'",
    max_exclusions = 3L,
    title = title,
    subtitle = chart_subtitle(series, name, level, measure, only_reporting),
    override_x_title = x_title,
    override_y_title = paste("Average daily", tolower(volume)),
    annotation_size = 4,
    upper_annotation_sf = 1.1,
    lower_annotation_sf = 0.9,
    flip_labels = FALSE,
    align_labels = TRUE,
    annotation_arrows = TRUE
  )
}


# The organisation's name in the latest period (names change over time)
latest_name <- function(series) {
  series |>
    slice_max(Period, n = 1, with_ties = FALSE) |>
    pull(Name)
}

restrict_to_period <- function(series, start_date, end_date) {
  series <- series |>
    filter(
      Period >= as.Date(start_date, tz = "Europe/London"),
      Period <= as.Date(end_date, tz = "Europe/London")
    )
  if (nrow(series) == 0) {
    stop("No data for provider period specified")
  }
  series
}

# e.g. "Region:London\nAll department types\nIncludes only providers that are
# still reporting"
chart_subtitle <- function(series, name, level, measure, only_reporting) {
  england <- series$Nat_Code[1] == "E"

  level_title <- switch(
    level,
    National = "Country:",
    Regional = if (england) "Region:" else "Board:",
    Provider = ""
  )

  # Only England's data distinguish department types
  type_title <- if (!england) {
    ""
  } else if (measure == "Typ1") {
    "\nType 1 departments only"
  } else {
    "\nAll department types"
  }

  # Only NHS England has providers that stopped reporting (see the
  # Understanding the analysis tab)
  reporting_title <- if (!england || level == "Provider") {
    ""
  } else if (only_reporting) {
    "\nIncludes only providers that are still reporting"
  } else {
    "\nIncludes providers that are no longer reporting"
  }

  paste0(level_title, name, type_title, reporting_title)
}
