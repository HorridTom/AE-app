# Shewhart charts of a series made by make_series(), drawn with autospc.

# The fewest periods used to calculate a centre line and limits: two years of
# monthly data, so that the limits cover the whole seasonal cycle, or
# autospc's default of 21 for weekly data.
period_min <- function(weekly) {
  if (weekly) 21L else 24L
}

#' Chart of the percentage of attendances within 4 hours (P' chart)
#'
#' @param series a data frame from make_series()
#' @param level,measure,only_reporting,weekly the values used to make the
#'   series, for the chart's subtitle and title
#' @param start_date,end_date the period analysed
#' @param show_from,show_to the dates shown. The chart is analysed on all the
#'   data from start_date to end_date, so the limits at any point don't depend
#'   on the dates shown.
#' @param x_title horizontal axis title
chart_performance <- function(
  series,
  level,
  measure = "All",
  only_reporting = FALSE,
  weekly = FALSE,
  start_date = "2015-07-01",
  end_date = Sys.Date(),
  show_from = start_date,
  show_to = end_date,
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
  window <- display_window(series$Period, show_from, show_to)

  autospc::autospc(
    series,
    x = Period,
    y = Within_4h,
    n = Total_Att,
    chart_type = "P'",
    max_exclusions = 3L,
    period_min = period_min(weekly),
    floating_median = "auto",
    display_window = window,
    title = "Percentage A&E attendances\nwith time in department < 4h",
    subtitle = chart_subtitle(series, name, level, measure, only_reporting),
    override_x_title = x_title,
    override_y_title = "Percentage within 4 hours",
    annotation_size = 4,
    upper_annotation_sf = 1.08,
    lower_annotation_sf = 0.92,
    flip_labels = TRUE,
    use_caption = FALSE
  ) |>
    fit_y_axis(upper_annotation_sf = 1.08, lower_annotation_sf = 0.92) |>
    date_axis(show_from, show_to)
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
  show_from = start_date,
  show_to = end_date,
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
  window <- display_window(
    series$Period[!is.na(series$total)],
    show_from,
    show_to
  )

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
    period_min = period_min(weekly),
    floating_median = "auto",
    display_window = window,
    title = title,
    subtitle = chart_subtitle(series, name, level, measure, only_reporting),
    override_x_title = x_title,
    override_y_title = paste("Average daily", tolower(volume)),
    annotation_size = 4,
    upper_annotation_sf = 1.1,
    lower_annotation_sf = 0.9,
    flip_labels = FALSE,
    align_labels = TRUE,
    annotation_arrows = TRUE,
    use_caption = FALSE
  ) |>
    fit_y_axis(
      upper_annotation_sf = 1.1,
      lower_annotation_sf = 0.9,
      from_zero = TRUE
    ) |>
    date_axis(show_from, show_to)
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

# e.g. "Region: London\nAll department types\nIncludes only providers that are
# still reporting"
chart_subtitle <- function(series, name, level, measure, only_reporting) {
  england <- series$Nat_Code[1] == "E"

  level_title <- switch(
    level,
    National = "Country: ",
    Regional = if (england) "Region: " else "Board: ",
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


# The dates to show, as autospc's display_window: padded by half the time
# between points, so that the first and last points shown are not cut in half,
# and no others are shown.
display_window <- function(periods, from, to) {
  from <- as.Date(from)
  to <- as.Date(to)
  if (!any(periods >= from & periods <= to)) {
    no_chart(
      "There is no data for this selection between ",
      format(from, "%B %Y"),
      " and ",
      format(to, "%B %Y"),
      "."
    )
  }
  half_period <- stats::median(diff(sort(unique(periods)))) / 2
  if (is.na(half_period)) {
    half_period <- as.difftime(15, units = "days")
  }
  c(from - half_period, to + half_period)
}

# Place the date axis labels for the dates shown. Keeps the axis limits
# autospc set, so that lines continue to the edges of the chart.
date_axis <- function(chart, from, to) {
  full_range <- as.Date(chart$scales$get_scales("x")$limits)
  shown <- c(as.Date(from), as.Date(to))
  breaks <- scales::breaks_pretty(n = 8)(shown)
  suppressMessages(
    chart +
      ggplot2::scale_x_date(
        labels = scales::label_date_short(),
        breaks = breaks[breaks >= shown[1] & breaks <= shown[2]],
        limits = full_range
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5)
      )
  )
}


# Fit the vertical axis to all the chart's data: its points, its control
# limits, and room for the centre line labels, which autospc places at the
# limits multiplied by upper_annotation_sf and lower_annotation_sf. With
# from_zero = TRUE (counts), the axis always starts at zero. Otherwise it
# starts at zero only where the data come close to it, and draw_chart() marks
# the axis with a break where it does not.
fit_y_axis <- function(
  chart,
  upper_annotation_sf,
  lower_annotation_sf,
  from_zero = FALSE
) {
  d <- as.data.frame(chart)
  low <- min(c(d$series, d$lcl * lower_annotation_sf), na.rm = TRUE)
  high <- max(c(d$series, d$ucl * upper_annotation_sf), na.rm = TRUE)
  padding <- 0.05 * (high - low)
  low <- low - padding
  high <- high + padding
  if (from_zero || low < 0.2 * high) {
    low <- 0
  }
  # Keep the dates autospc zoomed to (display_window); no further padding, as
  # the window and the vertical range are already padded
  suppressMessages(
    chart +
      ggplot2::coord_cartesian(
        xlim = chart$coordinates$limits$x,
        ylim = c(low, high),
        expand = FALSE
      )
  )
}


#' Draw a chart
#'
#' Draws a chart from chart_performance() or chart_volume(), marking the
#' vertical axis with a break where it does not start at zero.
#'
#' @param chart a chart from chart_performance() or chart_volume()
#' @param caption optional caption, e.g. from chart_caption()
draw_chart <- function(chart, caption = NULL) {
  if (!is.null(caption)) {
    chart <- chart +
      ggplot2::labs(caption = caption) +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(hjust = 0, size = 9)
      )
  }
  table <- ggplot2::ggplotGrob(chart)
  if (chart$coordinates$limits$y[1] > 0) {
    panel <- table$layout[table$layout$name == "panel", ]
    table <- gtable::gtable_add_grob(
      table,
      y_axis_break(),
      t = panel$t,
      l = panel$l,
      clip = "off",
      z = Inf,
      name = "y_axis_break"
    )
  }
  grid::grid.newpage()
  grid::grid.draw(table)
}

# A break mark on the vertical axis, near its foot: a short gap in the axis,
# between two slanted lines
y_axis_break <- function(height = grid::unit(4, "mm")) {
  half_width <- grid::unit(1.8, "mm")
  half_gap <- grid::unit(0.6, "mm")
  slant <- grid::unit(0.8, "mm")
  line <- function(offset) {
    grid::segmentsGrob(
      x0 = -half_width,
      x1 = half_width,
      y0 = height + offset - slant,
      y1 = height + offset + slant,
      gp = grid::gpar(col = "grey40", lwd = 1.2)
    )
  }
  grid::grobTree(
    grid::rectGrob(
      x = grid::unit(0, "npc"),
      y = height,
      width = half_width,
      height = 2 * half_gap,
      gp = grid::gpar(fill = "white", col = NA)
    ),
    line(-half_gap),
    line(half_gap)
  )
}


#' Caption for a downloaded chart
#'
#' So that a downloaded chart can be understood on its own: the chart type,
#' the rules, the data source with its licence, and when it was downloaded.
#'
#' @param chart_type "P'" or "U'"
#' @param country "England" or "Scotland"
#' @param date the date of the download
chart_caption <- function(chart_type, country, date = Sys.Date()) {
  source <- switch(
    country,
    England = "NHS England, A&E Attendances and Emergency Admissions",
    Scotland = "Public Health Scotland, Weekly A&E Activity and Waiting Times"
  )
  lines <- c(
    paste0(
      chart_type,
      " Shewhart chart. Rule 1: any point outside the control limits. ",
      "Rule 2: eight or more consecutive points all above, or all below, ",
      "the centre line."
    ),
    paste0(
      "Source: ",
      source,
      ". Contains public sector information licensed under the Open ",
      "Government Licence v3.0."
    ),
    paste0(
      "A&E Tracker, https://clahrcnwl.shinyapps.io/ae-app/, ",
      format(as.Date(date), "%d %B %Y"),
      "."
    )
  )
  paste(strwrap(lines, width = 120), collapse = "\n")
}


# Charts narrower than this, in pixels (e.g. on a phone), are laid out for a
# small screen by fit_to_width()
narrow_width <- 700

is_narrow <- function(width) {
  !is.null(width) && width < narrow_width
}

#' Lay a chart out for the width it is shown at
#'
#' On a narrow screen: the legend goes below the chart, titles wrap, text is
#' a little smaller, and the horizontal axis has fewer labels. Wider charts
#' are unchanged. Only the layout changes, not the analysis.
#'
#' @param chart a chart from chart_performance() or chart_volume()
#' @param width the width it is shown at, in pixels
fit_to_width <- function(chart, width) {
  if (!is_narrow(width)) {
    return(chart)
  }
  characters <- floor(width / 11)
  wrap <- function(text) {
    if (is.null(text)) {
      return(NULL)
    }
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    paste(unlist(lapply(lines, strwrap, width = characters)), collapse = "\n")
  }
  # The dates shown, without the room after them for the median's label
  shown <- as.Date(chart$coordinates$limits$x)
  shown[2] <- min(shown[2], max(chart$data$x))
  full_range <- as.Date(chart$scales$get_scales("x")$limits)
  breaks <- scales::breaks_pretty(n = max(3, floor(width / 90)))(shown)

  suppressMessages(
    chart +
      ggplot2::labs(
        title = wrap(chart$labels$title),
        subtitle = wrap(chart$labels$subtitle)
      ) +
      ggplot2::scale_x_date(
        labels = scales::label_date_short(),
        breaks = breaks[breaks >= shown[1] & breaks <= shown[2]],
        limits = full_range
      ) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.title.position = "top",
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        plot.title = ggplot2::element_text(size = 15),
        plot.subtitle = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(size = 11),
        axis.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 10),
        legend.text = ggplot2::element_text(size = 9)
      )
  )
}
