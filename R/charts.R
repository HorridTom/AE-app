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
  check_type_1(series, measure)

  # A percentage needs attendances: leave out periods with no 4-hour data, or
  # no attendances
  series <- series |> filter(!is.na(Within_4h), Total_Att > 0)
  if (nrow(series) == 0) {
    no_chart(
      "There is no data on attendances within 4 hours for this selection."
    )
  }
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

  check_type_1(series, measure)
  name <- latest_name(series)
  series <- restrict_to_period(series, start_date, end_date)

  # e.g. admissions for a provider with no type 1 department
  if (all(is.na(series[[total_col]]) | series[[total_col]] == 0)) {
    no_chart(switch(
      volume,
      Attendances = "No attendances are reported for this selection.",
      Admissions = "No emergency admissions via A&E are reported for this selection."
    ))
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
  data_end <- series$Data_End[1]
  scotland <- series$Nat_Code[1] == "S"
  series <- series |>
    filter(
      Period >= as.Date(start_date, tz = "Europe/London"),
      Period <= as.Date(end_date, tz = "Europe/London")
    )
  if (nrow(series) == 0) {
    no_chart(
      "There is no data for this selection between ",
      format(as.Date(start_date), "%B %Y"),
      " and ",
      format(as.Date(end_date), "%B %Y"),
      ". The latest data is for ",
      format_period(data_end, weekly = scotland),
      "."
    )
  }
  series
}

# Where every attendance in the period was within 4 hours, the performance
# chart is a line at 100% with no control limits. This explains why, for
# showing with the chart. "" otherwise.
all_within_4h_note <- function(
  series,
  weekly = FALSE,
  start_date = "2015-07-01",
  end_date = Sys.Date()
) {
  series <- series |>
    filter(
      !is.na(Within_4h),
      Total_Att > 0,
      Period >= as.Date(start_date, tz = "Europe/London"),
      Period <= as.Date(end_date, tz = "Europe/London")
    )
  if (nrow(series) == 0 || any(series$Greater_4h > 0)) {
    return("")
  }
  paste0(
    "Every attendance was within 4 hours in every ",
    if (weekly) "week" else "month",
    " from ",
    format_period(min(series$Period), weekly),
    " to ",
    format_period(max(series$Period), weekly),
    ", so there is no variation from which to calculate control limits."
  )
}

# Type 1 departments are major A&E departments. Many providers (e.g. walk-in
# and urgent treatment centres) have none.
check_type_1 <- function(series, measure) {
  if (
    measure == "Typ1" && all(is.na(series$Total_Att) | series$Total_Att == 0)
  ) {
    no_chart(
      "There are no type 1 (major A&E department) attendances for this ",
      "selection. Untick \"Only include type 1 departments\" to see all ",
      "attendances."
    )
  }
}

# e.g. "July 2017", or "the week ending 3 May 2015"
format_period <- function(date, weekly = FALSE) {
  if (weekly) {
    paste(
      "the week ending",
      as.integer(format(date, "%d")),
      format(date, "%B %Y")
    )
  } else {
    format(date, "%B %Y")
  }
}

# Stop, with a message for the app's user explaining why there is no chart
no_chart <- function(...) {
  stop(structure(
    class = c("no_chart", "error", "condition"),
    list(message = paste0(...), call = NULL)
  ))
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
