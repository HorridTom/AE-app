# Making the app quicker to start, and timing it.
#
# shinyapps.io stops the app's instance when nobody has used it for a while,
# so the next visitor waits for a new instance to start R, run app.R and draw
# the first charts. These help to see where that time goes, and to shorten it.

# Write a line to the log with the time since R started, e.g.
#   [timing]   4.21s data read
# The lines can be read with rsconnect::showLogs().
log_timing <- function(event) {
  # Not while testing
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    return(invisible())
  }
  message(sprintf("[timing] %7.2fs %s", proc.time()[["elapsed"]], event))
}

# Load the packages that the charts need, which R otherwise loads only when
# the first chart is analysed: autospc and its imports (ggplot2 and others),
# and ggrepel and ggpp, which autospc only suggests. On a new instance this
# takes a few seconds. app.R schedules it for as soon as the app is listening,
# so that it runs while the visitor's browser is loading the page.
preload_chart_packages <- function() {
  for (pkg in c("autospc", "ggrepel", "ggpp")) {
    if (requireNamespace(pkg, quietly = TRUE)) loadNamespace(pkg)
  }
  log_timing("chart packages loaded")
}
