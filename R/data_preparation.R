# Preparing the A&E data for analysis.
#
# The England and Scotland data are put into one data frame with the same
# columns, one row per provider (England) or hospital (Scotland) per period:
#
#   Country, Nat_Code   country name and code ("England"/"E", "Scotland"/"S")
#   Region, Reg_Code    NHS England region, or Scottish health board
#   Prov_Name, Prov_Code
#   Period              first day of the month (England), last day of the
#                       week (Scotland)
#   Att_All, Att_All_Br         attendances, and attendances over 4 hours
#   Att_Typ1, Att_Typ1_Br       the same for type 1 departments (England only)
#   E_Adm_All_ED, E_Adm_Typ1    emergency admissions via A&E (England only)

prepare_ae_data <- function(england, scotland) {
  dplyr::bind_rows(standardise_england(england), standardise_scotland(scotland))
}

ae_data_columns <- c(
  "Country",
  "Nat_Code",
  "Region",
  "Reg_Code",
  "Prov_Name",
  "Prov_Code",
  "Period",
  "Att_All",
  "Att_All_Br",
  "Att_Typ1",
  "Att_Typ1_Br",
  "E_Adm_All_ED",
  "E_Adm_Typ1"
)

# NHS England's region names have changed over the years. Group them into four
# regions, with made-up codes that must differ from the Scottish board codes.
standardise_england <- function(england) {
  england |>
    mutate(
      Region = case_when(
        str_detect(Region, coll("london", ignore_case = TRUE)) ~ "London",
        str_detect(Region, coll("midlands", ignore_case = TRUE)) ~ "Midlands",
        str_detect(
          Region,
          coll("north", ignore_case = TRUE)
        ) ~ "North of England",
        .default = "South of England"
      ),
      Reg_Code = unname(c(
        "London" = "Lo",
        "Midlands" = "Mi",
        "North of England" = "No",
        "South of England" = "So"
      )[Region]),
      Country = "England",
      Nat_Code = "E",
      Period = Month_Start
    ) |>
    select(all_of(ae_data_columns))
}

# Scotland has no type 1 breakdown and no admissions data.
standardise_scotland <- function(scotland) {
  scotland |>
    mutate(
      Region = gsub("NHS ", "", Board_Name),
      Reg_Code = Board_Code,
      Att_All_Br = Att_4hr_Br,
      Country = "Scotland",
      Nat_Code = "S",
      Period = Week_End,
      Att_Typ1 = NA_real_,
      Att_Typ1_Br = NA_real_,
      E_Adm_All_ED = NA_real_,
      E_Adm_Typ1 = NA_real_
    ) |>
    select(all_of(ae_data_columns))
}


# One row per provider/hospital, with its region and country, taken from its
# latest row of data, so that it has its current name. Sorted by name. Used to
# fill the menus and to look up the organisation chosen.
#
# Menu_Name is the name shown in the menu: the provider's name, followed by
# its code where another provider in the same country has the same name (e.g.
# a walk-in centre that has moved to a new code).
organisation_lookup <- function(ae_data) {
  ae_data |>
    arrange(desc(Period)) |>
    distinct(Prov_Code, .keep_all = TRUE) |>
    select(Prov_Code, Prov_Name, Reg_Code, Region, Nat_Code, Country) |>
    arrange(Prov_Name, Prov_Code) |>
    mutate(
      Menu_Name = if (n() > 1) {
        paste0(Prov_Name, " (", Prov_Code, ")")
      } else {
        Prov_Name
      },
      .by = c(Country, Prov_Name)
    )
}

# Choices for the provider menu: the names shown, with the codes as values
provider_choices <- function(organisations, country) {
  in_country <- organisations[organisations$Country == country, ]
  stats::setNames(in_country$Prov_Code, in_country$Menu_Name)
}

region_names <- function(organisations, country) {
  levels(factor(organisations$Region[organisations$Country == country]))
}

country_names <- function(organisations) {
  levels(factor(organisations$Country))
}

# The code of the region or board with the given name, or of the country
organisation_code <- function(organisations, country, level, name = NULL) {
  in_country <- organisations[organisations$Country == country, ]
  switch(
    level,
    Regional = in_country$Reg_Code[in_country$Region == name][1],
    National = in_country$Nat_Code[1]
  )
}

# The current name of the provider with the given code
provider_name <- function(organisations, code) {
  organisations$Prov_Name[organisations$Prov_Code == code][1]
}

# The names a provider has had before its current one, with the first and
# last period each was used: a data frame with columns Prov_Name, First and
# Last, earliest first. No rows if the provider has only had one name.
earlier_names <- function(ae_data, code) {
  ae_data |>
    filter(Prov_Code == code) |>
    summarise(First = min(Period), Last = max(Period), .by = Prov_Name) |>
    arrange(First) |>
    filter(Last < max(Last))
}

# e.g. "Previously: 5 Boroughs Partnership NHS Foundation Trust (July 2015 to
# March 2016)." "" if the provider has only had one name.
earlier_names_note <- function(ae_data, code) {
  names <- earlier_names(ae_data, code)
  if (nrow(names) == 0) {
    return("")
  }
  month <- function(date) format(date, "%B %Y")
  periods <- ifelse(
    names$First == names$Last,
    month(names$First),
    paste(month(names$First), "to", month(names$Last))
  )
  paste0(
    "Previously: ",
    paste0(names$Prov_Name, " (", periods, ")", collapse = "; "),
    "."
  )
}


# Providers whose code changed: other codes in the same country with the same
# current name, whose data come entirely before (previous) or after (next)
# this code's data. This is typically a service re-contracted to a new
# provider under a new code. Codes with the same name whose data overlap in
# time are different services, so are not linked.
#
# Returns a list with elements previous and next, each NULL or a one-row data
# frame with columns Prov_Code, First and Last (first and last periods).
code_handovers <- function(ae_data, organisations, code) {
  this_org <- organisations[organisations$Prov_Code == code, ]
  same_name <- organisations$Prov_Code[
    organisations$Country == this_org$Country &
      organisations$Prov_Name == this_org$Prov_Name &
      organisations$Prov_Code != code
  ]
  none <- list(previous = NULL, `next` = NULL)
  if (length(same_name) == 0) {
    return(none)
  }

  periods <- ae_data |>
    filter(Prov_Code %in% c(code, same_name)) |>
    summarise(First = min(Period), Last = max(Period), .by = Prov_Code)
  this_code <- periods[periods$Prov_Code == code, ]
  others <- periods[periods$Prov_Code != code, ]

  previous <- others |>
    filter(Last < this_code$First) |>
    slice_max(Last, n = 1, with_ties = FALSE)
  following <- others |>
    filter(First > this_code$Last) |>
    slice_min(First, n = 1, with_ties = FALSE)

  list(
    previous = if (nrow(previous) > 0) previous,
    `next` = if (nrow(following) > 0) following
  )
}
