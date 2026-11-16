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

# The app's UI as a function of the request, which Shiny calls for each page
# request. The first request also starts warm_up(), once the page has been
# sent: R does one thing at a time, so warming up any earlier could hold up
# the page. The warm-up then overlaps with the browser loading the page's
# scripts and styles (which Shiny serves without waiting for R) and
# connecting.
warm_up_on_first_request <- function(ui, ae_data) {
  started <- FALSE
  function(req) {
    log_timing("page requested")
    if (!started) {
      started <<- TRUE
      warm_up(ae_data)
    }
    ui
  }
}

# Set when a session starts, which stops the warm-up (see warm_up())
warm_up_state <- new.env()
warm_up_state$stop <- FALSE

stop_warm_up <- function() {
  warm_up_state$stop <- TRUE
}

# Do in advance what is slow the first time it is done in a new R process:
# loading the packages the charts need, and the first run of the code that
# makes a series, analyses it and draws the chart (R loads and compiles code
# as it is first used). Together these are about half the time of the first
# chart in a new process. It uses one provider's series, which is quick to
# analyse.
#
# The steps run one at a time, each scheduled after the last, so that a
# browser connecting can start its session in between (R does one thing at a
# time). Once a session has started, the remaining steps are skipped: the
# session's own first chart does them anyway.
warm_up <- function(ae_data, code = "RQM") {
  env <- new.env()
  steps <- list(
    "chart packages loaded" = function() {
      # autospc and its imports (ggplot2 and others), and ggrepel and ggpp,
      # which autospc only suggests
      for (pkg in c("autospc", "ggrepel", "ggpp")) {
        if (requireNamespace(pkg, quietly = TRUE)) loadNamespace(pkg)
      }
    },
    "series made" = function() {
      env$series <- make_series(ae_data, "Provider", code, "All")
    },
    "chart analysed" = function() {
      env$chart <- chart_performance(env$series, "Provider", "All")
    },
    "chart drawn" = function() {
      file <- tempfile(fileext = ".png")
      grDevices::png(file, width = 1348, height = 800, res = 144)
      on.exit({
        grDevices::dev.off()
        unlink(file)
      })
      draw_chart(fit_to_width(env$chart, 674))
    }
  )

  run_step <- function(i) {
    if (i > length(steps)) {
      return()
    }
    if (warm_up_state$stop) {
      log_timing("warm-up stopped: a session has started")
      return()
    }
    ok <- tryCatch(
      {
        steps[[i]]()
        log_timing(paste("warm-up:", names(steps)[i]))
        TRUE
      },
      error = function(e) {
        log_timing(paste("warm-up failed:", conditionMessage(e)))
        FALSE
      }
    )
    # A short delay lets Shiny deal with any waiting connection first
    if (ok) later::later(function() run_step(i + 1), delay = 0.01)
  }

  later::later(function() run_step(1), delay = 0.01)
  invisible()
}
