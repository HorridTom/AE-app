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


# One row per provider/hospital, with its region and country, taken from the
# first row of data for it, sorted by name. Used to fill the menus and to look
# up the code for the organisation chosen.
organisation_lookup <- function(ae_data) {
  ae_data |>
    distinct(Prov_Code, .keep_all = TRUE) |>
    select(Prov_Code, Prov_Name, Reg_Code, Region, Nat_Code, Country) |>
    arrange(Prov_Name)
}

# Choices for the menus
provider_names <- function(organisations, country) {
  organisations$Prov_Name[organisations$Country == country]
}

region_names <- function(organisations, country) {
  levels(factor(organisations$Region[organisations$Country == country]))
}

country_names <- function(organisations) {
  levels(factor(organisations$Country))
}

# The code of the organisation with the given name, at the given level
organisation_code <- function(organisations, country, level, name = NULL) {
  in_country <- organisations[organisations$Country == country, ]
  switch(
    level,
    Provider = in_country$Prov_Code[in_country$Prov_Name == name][1],
    Regional = in_country$Reg_Code[in_country$Region == name][1],
    National = in_country$Nat_Code[1]
  )
}
