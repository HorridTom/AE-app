# Building the time series for one organisation.

#' Time series for one organisation
#'
#' @param ae_data data prepared by prepare_ae_data()
#' @param level "National", "Regional" or "Provider"
#' @param code the organisation's code: Nat_Code, Reg_Code or Prov_Code
#' @param measure "All" (all department types) or "Typ1" (type 1 departments
#'   only). England only: Scotland's data are always for all types.
#' @param weekly Scotland only: TRUE for weekly data as published, FALSE for
#'   weekly data apportioned to months.
#' @param only_reporting England only, national and regional levels: TRUE to
#'   leave out providers that are missing 4-hour data for any period.
#'
#' @return A data frame with one row per period and columns Code, Name,
#'   Nat_Code, Period, Within_4h, Greater_4h, Total_Att, Total_Adm,
#'   Performance (proportion within 4 hours), days_in_period, daily_ave_att and
#'   daily_ave_adm, and Data_End: the last period of the data (for Scotland,
#'   the end of the last week, even where weeks are apportioned to months).
make_series <- function(
  ae_data,
  level,
  code,
  measure = "All",
  weekly = FALSE,
  only_reporting = FALSE
) {
  totals <- organisation_totals(ae_data, level, code, only_reporting)
  scotland <- identical(totals$Nat_Code[1], "S")

  if (scotland || measure == "All") {
    series <- totals |>
      select(
        Code,
        Period,
        Name,
        Nat_Code,
        Within_4h = Att_All_NotBr,
        Greater_4h = Att_All_Br,
        Total_Att = Att_All,
        Total_Adm = E_Adm_All_ED
      )
  } else {
    series <- totals |>
      select(
        Code,
        Period,
        Name,
        Nat_Code,
        Within_4h = Att_Typ1_NotBr,
        Greater_4h = Att_Typ1_Br,
        Total_Att = Att_Typ1,
        Total_Adm = E_Adm_Typ1
      )
  }

  weekly <- scotland && weekly
  # The end of the data, before any weeks are apportioned to months
  data_end <- max(as.Date(series$Period))
  if (scotland && !weekly) {
    series <- weekly_to_monthly(series)
  }

  series |>
    mutate(
      Performance = Within_4h / Total_Att,
      Period = as.Date(Period, tz = "Europe/London"),
      Data_End = data_end
    ) |>
    arrange(Period) |>
    mutate(
      days_in_period = if (weekly) {
        7
      } else {
        as.numeric(lubridate::days_in_month(Period))
      },
      daily_ave_att = Total_Att / days_in_period,
      daily_ave_adm = Total_Adm / days_in_period
    )
}


# Totals per period for the organisation with `code` at `level`, adding up its
# providers.
organisation_totals <- function(ae_data, level, code, only_reporting = FALSE) {
  name_col <- switch(
    level,
    National = "Country",
    Regional = "Region",
    Provider = "Prov_Name"
  )
  code_col <- switch(
    level,
    National = "Nat_Code",
    Regional = "Reg_Code",
    Provider = "Prov_Code"
  )

  df <- ae_data |>
    mutate(Name = .data[[name_col]], Code = .data[[code_col]]) |>
    filter(Code == code)

  # Leave out providers that stopped reporting 4-hour data, for the whole
  # period, so that like is compared with like
  if (only_reporting && level != "Provider") {
    not_reporting <- df |>
      filter(is.na(Att_All_Br)) |>
      distinct(Prov_Code) |>
      pull(Prov_Code)
    df <- df |> filter(!(Prov_Code %in% not_reporting))
  }

  # Missing attendances count as zero; missing breaches and admissions make
  # the total missing.
  df |>
    group_by(Name, Code, Period) |>
    summarise(
      across(c(Att_Typ1, Att_All), ~ sum(.x, na.rm = TRUE)),
      across(c(Att_Typ1_Br, Att_All_Br, E_Adm_Typ1, E_Adm_All_ED), sum),
      Nat_Code = first(Nat_Code),
      .groups = "drop"
    ) |>
    mutate(
      Att_Typ1_NotBr = Att_Typ1 - Att_Typ1_Br,
      Att_All_NotBr = Att_All - Att_All_Br
    )
}


# Apportion weekly counts (Scotland) to calendar months. The first and last
# months are dropped, because they are only partly covered by the weeks.
weekly_to_monthly <- function(series) {
  series <- series |>
    mutate(Period = as.Date(Period)) |>
    arrange(Period)
  # Period is the last day of each week
  first_day <- as.character(series$Period[1] - lubridate::weeks(1))

  to_months <- function(x) {
    wktmo::weekToMonth(
      x,
      datStart = first_day,
      wkMethod = "startDat",
      format = "%Y-%m-%d"
    )
  }
  totals <- to_months(series$Total_Att)

  monthly <- data.frame(
    Period = totals$yearMonth,
    Within_4h = to_months(series$Within_4h)$value,
    Greater_4h = to_months(series$Greater_4h)$value,
    Total_Att = totals$value,
    Total_Adm = to_months(series$Total_Adm)$value
  )

  last <- nrow(series)
  monthly |>
    mutate(
      Code = series$Code[last],
      Name = series$Name[last],
      Nat_Code = series$Nat_Code[last]
    ) |>
    filter(row_number() != 1 & row_number() != nrow(monthly)) |>
    mutate(Period = zoo::as.Date(zoo::as.yearmon(Period)))
}
