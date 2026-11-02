# Loading the A&E data.
#
# The data is kept as CSV files in the AE-data repository on GitHub, which a
# scheduled GitHub Action keeps up to date (see
# https://github.com/HorridTom/AE-data). The app reads the files once, when it
# starts.
#
# To read the files from somewhere else, e.g. a local copy for development,
# set the environment variable AE_DATA_SOURCE to a folder or a URL holding
# AE_Data_England.csv and AE_Data_Scotland.csv:
#
#   Sys.setenv(AE_DATA_SOURCE = "~/Rprojects/AE-data/data")
#   shiny::runApp()
#
# If the files can't be downloaded, the app falls back to copies in the
# data/ folder of the app, if there are any (they are not kept in git). Each
# successful download refreshes those copies.

default_data_source <- "https://raw.githubusercontent.com/HorridTom/AE-data/main/data"
local_data_dir <- "data"

ae_col_types <- list(
  England = readr::cols(
    Prov_Code = readr::col_character(),
    Region = readr::col_character(),
    Prov_Name = readr::col_character(),
    Month_Start = readr::col_date(),
    SourceFile = readr::col_character(),
    .default = readr::col_double()
  ),
  Scotland = readr::cols(
    Week_End = readr::col_date(),
    Board_Code = readr::col_character(),
    Board_Name = readr::col_character(),
    Prov_Code = readr::col_character(),
    Prov_Name = readr::col_character(),
    SourceFile = readr::col_character(),
    .default = readr::col_double()
  )
)

ae_file_name <- function(country) {
  paste0("AE_Data_", country, ".csv")
}

# Read one country's data from `source` (a folder or URL), falling back to the
# local copy. Returns a data.frame without the SourceFile column.
read_ae_file <- function(country, source = Sys.getenv("AE_DATA_SOURCE",
                                                       default_data_source)) {
  location <- if (grepl("^https?://", source)) {
    paste(source, ae_file_name(country), sep = "/")
  } else {
    file.path(path.expand(source), ae_file_name(country))
  }
  local_copy <- file.path(local_data_dir, ae_file_name(country))

  df <- tryCatch(
    readr::read_csv(location, col_types = ae_col_types[[country]],
                    progress = FALSE),
    error = function(e) {
      if (!file.exists(local_copy) ||
          normalizePath(local_copy) == normalizePath(location, mustWork = FALSE)) {
        stop("Could not read the ", country, " A&E data from ", location,
             ": ", conditionMessage(e), call. = FALSE)
      }
      warning("Could not read the ", country, " A&E data from ", location,
              " (", conditionMessage(e), "). Using the copy in ", local_copy,
              ".", call. = FALSE)
      readr::read_csv(local_copy, col_types = ae_col_types[[country]],
                      progress = FALSE)
    }
  )

  # Keep a copy to fall back on next time. Failing to write it doesn't matter.
  if (grepl("^https?://", location)) {
    try({
      dir.create(local_data_dir, showWarnings = FALSE)
      readr::write_csv(df, local_copy, na = "")
    }, silent = TRUE)
  }

  df$SourceFile <- NULL
  as.data.frame(df)
}
